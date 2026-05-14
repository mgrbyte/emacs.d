;;; mgrbyte-claude.el --- Claude Code integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Matthew Russell

;; Author: Matthew Russell <matthew.russell@horizon5.org>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Claude Code integration for Emacs.
;;
;; External terminal integration:
;;   Runs Claude Code in alacritty/tmux for smooth rendering while
;;   Emacs handles IDE features (ediff, selection tracking, MCP tools)
;;   via the claude-code-ide.el websocket bridge.
;;
;; Auto-dependency discovery (planned):
;;   Discovers local Python dependencies from pyproject.toml and
;;   pre-configures --add-dir flags when switching projects.
;;
;; Requires claude-code-ide.el to be loaded first (for MCP server functions).

;;; Code:

(require 'cl-lib)
(require 'transient)

;;;; Minor mode and keymap

(defvar mgrbyte-claude-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c c") #'mgrbyte-claude-ide-external)
    (define-key map (kbd "C-c c m") #'mgrbyte-claude-ide-external-menu)
    (define-key map (kbd "C-c c r") #'mgrbyte-claude-ide-external-resume)
    (define-key map (kbd "C-c c k") #'mgrbyte-claude-ide-external-continue)
    (define-key map (kbd "C-c c s") #'mgrbyte-claude-ide-external-stop)
    (define-key map (kbd "C-c c l") #'mgrbyte-claude-ide-external-list)
    (define-key map (kbd "C-c c d") #'mgrbyte-claude-ide-external-in-directory)
    (define-key map (kbd "C-c c R") #'mgrbyte-claude-ide-external-resume-in-directory)
    (define-key map (kbd "C-c c a") #'mgrbyte-claude-ide-add-dir)
    (define-key map (kbd "C-c c w") #'mgrbyte-workon)
    (define-key map (kbd "C-c c p") #'mgrbyte-project-location)
    map)
  "Keymap for `mgrbyte-claude-mode'.")

;;;###autoload
(define-minor-mode mgrbyte-claude-mode
  "Toggle mgrbyte-claude mode.

\\{mgrbyte-claude-keymap}
Provides Claude Code IDE integration using an external terminal
for rendering with Emacs handling IDE features via MCP."
  :lighter (:eval (if (and (eq mgrbyte-project-location 'remote)
                         mgrbyte-project-remote-host)
                    (format " Claude[%s]" mgrbyte-project-remote-host)
                  " Claude"))
  :global t
  :keymap mgrbyte-claude-keymap
  :group 'mgrbyte)

;;;; External terminal integration

(defvar mgrbyte-claude-ide-external-script "claude-ide-external"
  "Name of the external launcher script (must be on PATH).")

;;;; Project location (local/remote)

(defvar-local mgrbyte-project-location nil
  "Project location: nil or `local' for local, `remote' for remote server.")
(put 'mgrbyte-project-location 'safe-local-variable
     (lambda (v) (memq v '(nil local remote))))

(defvar-local mgrbyte-project-remote-host nil
  "Remote hostname when `mgrbyte-project-location' is `remote'.")
(put 'mgrbyte-project-remote-host 'safe-local-variable #'stringp)

(defconst mgrbyte-remote-mcp-prompt-file
  (expand-file-name "remote-mcp-prompt.md"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the remote MCP prompt template.")

(defun mgrbyte-remote-mcp--translate-path (local-path)
  "Translate LOCAL-PATH from macOS to the remote home directory equivalent."
  (replace-regexp-in-string "^/Users/" "/home/" local-path))

(defun mgrbyte-remote-mcp--build-system-prompt (host working-dir)
  "Build a system prompt for remote MCP tools from template.
HOST is the remote hostname, WORKING-DIR is the project path on the remote."
  (with-temp-buffer
    (insert-file-contents mgrbyte-remote-mcp-prompt-file)
    (format (buffer-string) host working-dir
            host working-dir host)))

(defvar mgrbyte-claude-ide-external--sessions (make-hash-table :test 'equal)
  "Hash table mapping working directories to external session plists.
Each plist contains :port :session-id :window-name :working-dir.")

(defun mgrbyte-claude-ide-external--cleanup (working-dir)
  "Clean up session state for WORKING-DIR."
  (let ((session (gethash working-dir mgrbyte-claude-ide-external--sessions)))
    (when session
      (dolist (dir (plist-get session :add-dirs))
        (remhash dir claude-code-ide-mcp--sessions))
      (claude-code-ide-mcp-stop-session working-dir)
      (remhash working-dir mgrbyte-claude-ide-external--sessions)
      (message "Claude IDE external session ended for %s"
               (file-name-nondirectory (directory-file-name working-dir))))))

(defun mgrbyte-claude-ide-external (&optional resume continue)
  "Start Claude Code IDE with MCP integration in an external tmux window.
Starts the MCP servers in Emacs, then launches claude-ide-external
script in a new tmux window.
RESUME and CONTINUE control -r and -c flags respectively."
  (interactive)
  (let* ((working-dir (claude-code-ide--get-working-directory))
         (existing (gethash working-dir mgrbyte-claude-ide-external--sessions)))
    (when existing
      (user-error "Session already running for %s" working-dir))
    (let* ((session-id (format "claude-%s-%s"
                               (file-name-nondirectory (directory-file-name working-dir))
                               (format-time-string "%Y%m%d-%H%M%S")))
           (port (claude-code-ide-mcp-start working-dir))
           (mcp-config (when (claude-code-ide-mcp-server-ensure-server)
                         (claude-code-ide-mcp-server-get-config session-id)))
           (mcp-url (when mcp-config
                      (let ((servers (cdr (assq 'mcpServers mcp-config))))
                        (cdr (assq 'url (cdr (assq 'emacs-tools servers)))))))
           (allowed-tools (when mcp-config
                            (cond
                             ((eq claude-code-ide-mcp-allowed-tools 'auto)
                              (mapconcat #'identity
                                         (claude-code-ide-mcp-server-get-tool-names "mcp__emacs-tools__")
                                         " "))
                             ((listp claude-code-ide-mcp-allowed-tools)
                              (mapconcat #'identity claude-code-ide-mcp-allowed-tools " "))
                             (t claude-code-ide-mcp-allowed-tools))))
           (window-name (format "claude:%s"
                                (file-name-nondirectory
                                 (directory-file-name working-dir))))
           (add-dir-dirs (mgrbyte-claude--resolve-add-dir-dirs working-dir))
           (script-args (list mgrbyte-claude-ide-external-script
                              "--port" (number-to-string port)
                              "--session-id" session-id
                              "--mcp-url" (or mcp-url "")
                              "--working-dir" working-dir)))
      (when resume (setq script-args (append script-args (list "--resume"))))
      (when continue (setq script-args (append script-args (list "--continue"))))
      (when allowed-tools
        (setq script-args (append script-args (list "--allowed-tools" allowed-tools))))
      (when add-dir-dirs
        (setq script-args
              (append script-args
                      (list "--extra-flags"
                            (mgrbyte-claude--build-add-dir-flags add-dir-dirs)))))
      ;; Remote mode: inject system prompt for remote MCP tools
      (when (eq mgrbyte-project-location 'remote)
        (let ((remote-prompt (mgrbyte-remote-mcp--build-system-prompt
                              mgrbyte-project-remote-host
                              (mgrbyte-remote-mcp--translate-path working-dir))))
          (setq script-args (append script-args
                                    (list "--remote-prompt" remote-prompt)))))
      ;; Kill any stale tmux window
      (ignore-errors
        (call-process "tmux" nil nil nil "kill-window" "-t" window-name))
      ;; Launch in tmux — pass script and args directly (no shell quoting)
      (apply #'call-process "tmux" nil nil nil
             "new-window" "-n" window-name script-args)
      ;; Select the new tmux window and focus Alacritty
      (call-process "tmux" nil nil nil "select-window" "-t" window-name)
      (cond
       ((eq system-type 'darwin)
        (call-process "osascript" nil nil nil "-e"
                      "tell application \"Alacritty\" to activate"))
       ((eq system-type 'gnu/linux)
        ;; focus-window@mgrbyte GNOME Shell extension provides this interface
        (call-process "gdbus" nil nil nil
                      "call" "--session"
                      "--dest" "org.gnome.Shell"
                      "--object-path" "/org/mgrbyte/FocusWindow"
                      "--method" "org.mgrbyte.FocusWindow.FocusWindowByClass"
                      "Alacritty")))
      ;; Notify MCP tools server about the session
      (claude-code-ide-mcp-server-session-started session-id working-dir nil)
      ;; Track the session (including resolved add-dirs)
      (let ((session-plist (list :port port :session-id session-id
                                 :window-name window-name :working-dir working-dir
                                 :add-dirs add-dir-dirs)))
        (puthash working-dir session-plist mgrbyte-claude-ide-external--sessions)
        ;; Register add-dirs with MCP so ediff works for files in those paths
        (when add-dir-dirs
          (mgrbyte-claude-ide-register-add-dirs working-dir add-dir-dirs)))
      (message "Claude IDE started in tmux window '%s' (port %d)" window-name port))))

(defun mgrbyte-claude-ide-external-resume ()
  "Start Claude Code IDE in external terminal with -r (resume) flag."
  (interactive)
  (mgrbyte-claude-ide-external t nil))

(defun mgrbyte-claude-ide-external-continue ()
  "Start Claude Code IDE in external terminal with -c (continue) flag."
  (interactive)
  (mgrbyte-claude-ide-external nil t))

(defun mgrbyte-claude-ide-external-stop ()
  "Stop the external Claude IDE session for the current project."
  (interactive)
  (let ((working-dir (claude-code-ide--get-working-directory)))
    (mgrbyte-claude-ide-external--cleanup working-dir)))

(defun mgrbyte-claude-ide-external-list ()
  "List active external Claude IDE sessions via completing-read.
Selecting a session switches to its tmux window."
  (interactive)
  (if (= (hash-table-count mgrbyte-claude-ide-external--sessions) 0)
      (message "No active external Claude IDE sessions")
    (let ((candidates '()))
      (maphash (lambda (dir plist)
                 (push (cons (format "%s (port %d)"
                                     (abbreviate-file-name dir)
                                     (plist-get plist :port))
                             plist)
                       candidates))
               mgrbyte-claude-ide-external--sessions)
      (let* ((choice (completing-read "Claude IDE session: "
                                       (mapcar #'car candidates) nil t))
             (plist (cdr (assoc choice candidates)))
             (window-name (plist-get plist :window-name)))
        (when window-name
          (call-process "tmux" nil nil nil "select-window" "-t" window-name))))))

(defun mgrbyte-claude-ide-external-in-directory ()
  "Start Claude Code IDE in an external terminal for a prompted directory."
  (interactive)
  (let ((default-directory (read-directory-name "Project directory: ")))
    (mgrbyte-claude-ide-external)))

(defun mgrbyte-claude-ide-external-resume-in-directory ()
  "Resume Claude Code IDE in an external terminal for a prompted directory."
  (interactive)
  (let ((default-directory (read-directory-name "Project directory: ")))
    (mgrbyte-claude-ide-external-resume)))

;;;; Project location management

(defun mgrbyte-claude--set-project-location (project-root location &optional host)
  "Write PROJECT-ROOT's location metadata to its .dir-locals.el.
LOCATION is a symbol (`local' or `remote').
HOST is the remote hostname (required when LOCATION is `remote')."
  (let* ((dir-locals-file (expand-file-name ".dir-locals.el" project-root))
         (existing (mgrbyte-claude--read-dir-locals dir-locals-file))
         (nil-alist (or (cdr (assq nil existing)) '()))
         (nil-alist (cons (cons 'mgrbyte-project-location location)
                          (assq-delete-all 'mgrbyte-project-location nil-alist))))
    (when (eq location 'remote)
      (setq nil-alist (cons (cons 'mgrbyte-project-remote-host host)
                            (assq-delete-all 'mgrbyte-project-remote-host nil-alist))))
    (when (eq location 'local)
      (setq nil-alist (assq-delete-all 'mgrbyte-project-remote-host nil-alist)))
    (mgrbyte-claude--write-dir-locals dir-locals-file nil-alist)))

(defun mgrbyte-project-location ()
  "Set the project location (local or remote) for the current project.
Writes to .dir-locals.el and applies immediately."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (unless project-root
      (user-error "Not in a project"))
    (let* ((location (intern (helm-comp-read "Project location: " '("local" "remote"))))
           (host (when (eq location 'remote)
                   (let* ((dir-locals-file (expand-file-name ".dir-locals.el" project-root))
                          (existing (mgrbyte-claude--read-dir-locals dir-locals-file))
                          (nil-alist (cdr (assq nil existing))))
                     (read-string "Remote hostname: "
                                  (cdr (assq 'mgrbyte-project-remote-host nil-alist)))))))
      (mgrbyte-claude--set-project-location project-root location host)
      (hack-dir-local-variables-non-file-buffer)
      (message "Project location set to %s%s"
               location
               (if host (format " (%s)" host) "")))))

(defun mgrbyte-claude--project-child-dirs (dir)
  "Return immediate child directories of DIR that are git repositories."
  (cl-remove-if-not
   (lambda (child)
     (and (file-directory-p child)
          (file-directory-p (expand-file-name ".git" child))))
   (directory-files dir t "^[^.]")))

(defun mgrbyte-workon ()
  "Select a directory and configure its project location (local/remote).
Prompts for a directory with helm.  If the directory is a project,
sets its location directly.  If not, applies the chosen location to
each immediate child directory that is a git repository."
  (interactive)
  (let* ((dir (file-name-as-directory
               (expand-file-name
                (helm-read-file-name "Work on directory: "
                                     :initial-input default-directory))))
         (is-project (file-directory-p (expand-file-name ".git" dir)))
         (targets (if is-project
                      (list dir)
                    (mgrbyte-claude--project-child-dirs dir))))
    (unless targets
      (user-error "No project directories found in %s" (abbreviate-file-name dir)))
    (let* ((location (intern (helm-comp-read
                              (if is-project
                                  "Project location: "
                                (format "Location for %d projects: "
                                        (length targets)))
                              '("local" "remote"))))
           (host (when (eq location 'remote)
                   (read-string "Remote hostname: "))))
      (dolist (target targets)
        (mgrbyte-claude--set-project-location target location host))
      (hack-dir-local-variables-non-file-buffer)
      (message "%s set to %s%s"
               (if is-project
                   (abbreviate-file-name dir)
                 (format "%d projects in %s"
                         (length targets)
                         (abbreviate-file-name dir)))
               location
               (if host (format " (%s)" host) "")))))

;;;###autoload (autoload 'mgrbyte-claude-ide-external-menu "mgrbyte-claude" nil t)
(transient-define-prefix mgrbyte-claude-ide-external-menu ()
  "Claude Code IDE commands."
  ["Session"
   ("c" "Start" mgrbyte-claude-ide-external)
   ("r" "Resume" mgrbyte-claude-ide-external-resume)
   ("k" "Continue" mgrbyte-claude-ide-external-continue)
   ("s" "Stop" mgrbyte-claude-ide-external-stop)
   ("l" "List sessions" mgrbyte-claude-ide-external-list)]
  ["Directory"
   ("d" "Start in directory" mgrbyte-claude-ide-external-in-directory)
   ("R" "Resume in directory" mgrbyte-claude-ide-external-resume-in-directory)
   ("a" "Add directory" mgrbyte-claude-ide-add-dir)]
  ["Project"
   ("w" "Work on project" mgrbyte-workon)
   ("p" "Set location (local/remote)" mgrbyte-project-location)])

;;;; Add-dir MCP session registration

(defun mgrbyte-claude-ide-register-add-dirs (primary-dir dirs)
  "Register DIRS as additional paths for PRIMARY-DIR's MCP session.
Each directory in DIRS gets an entry in `claude-code-ide-mcp--sessions'
pointing to the same session object as PRIMARY-DIR.
Also invalidates buffer caches for buffers under the new directories."
  (when-let ((session (gethash primary-dir claude-code-ide-mcp--sessions)))
    (let ((registered '()))
      (dolist (dir dirs)
        (let ((expanded (expand-file-name dir)))
          (unless (gethash expanded claude-code-ide-mcp--sessions)
            (puthash expanded session claude-code-ide-mcp--sessions)
            (push expanded registered))))
      (when registered
        (mgrbyte-claude-ide--invalidate-caches-for-dirs registered)
        (message "mgrbyte-claude: registered %d add-dir paths with MCP session"
                 (length registered))))))

(defun mgrbyte-claude-ide--invalidate-caches-for-dirs (dirs)
  "Invalidate buffer-local MCP caches for buffers under DIRS."
  (dolist (buffer (buffer-list))
    (when-let ((file (buffer-file-name buffer)))
      (let ((expanded-file (expand-file-name file)))
        (when (cl-some (lambda (dir)
                         (string-prefix-p (expand-file-name dir) expanded-file))
                       dirs)
          (with-current-buffer buffer
            (setq claude-code-ide-mcp--buffer-cache-valid nil)))))))

(defun mgrbyte-claude-ide-add-dir (dir)
  "Register DIR with the current project's MCP session and CLI.
Registers the directory in the Emacs MCP sessions hash so ediff
works for files in DIR, and sends /add-dir to the Claude Code CLI
so it knows to use openDiff for files there.
Run this when the CLI is idle at the prompt."
  (interactive "DAdd directory: ")
  (let* ((working-dir (claude-code-ide--get-working-directory))
         (session-plist (gethash working-dir mgrbyte-claude-ide-external--sessions)))
    (unless session-plist
      (user-error "No active session for %s" working-dir))
    (let ((expanded (expand-file-name dir))
          (window-name (plist-get session-plist :window-name)))
      ;; Register in Emacs MCP sessions hash
      (mgrbyte-claude-ide-register-add-dirs working-dir (list expanded))
      ;; Track in external session plist for cleanup
      (puthash working-dir
               (plist-put session-plist :add-dirs
                          (cons expanded (plist-get session-plist :add-dirs)))
               mgrbyte-claude-ide-external--sessions)
      ;; Send /add-dir to the Claude Code CLI via tmux
      (call-process "tmux" nil nil nil
                    "send-keys" "-t" window-name
                    (format "/add-dir %s" expanded) "Enter"))))

;;;; Auto-dependency discovery

(require 'toml)
(require 'projectile)

(defvar mgrbyte-claude-dep-name-prefixes '("techiaith")
  "Dependency name prefixes that indicate locally-maintained libraries.")

(defvar mgrbyte-claude-max-dep-depth 3
  "Maximum recursion depth for transitive dependency resolution.")

(defvar-local mgrbyte-claude-library-locations nil
  "List of parent directories containing local library projects.
Set via .dir-locals.el per project.")

(defvar-local mgrbyte-claude-pyproject-path nil
  "Relative path to pyproject.toml within the project root.
Defaults to \"pyproject.toml\" when nil.  Set via .dir-locals.el
for projects where the manifest is not at root (e.g. \"backend/pyproject.toml\").")

(defun mgrbyte-claude--strip-version-spec (dep-string)
  "Strip version specifiers from DEP-STRING, returning the package name."
  (car (split-string dep-string "[><=!;\\[\\]  ]" t)))

(defun mgrbyte-claude--matches-prefix-p (name)
  "Return non-nil if NAME starts with any prefix in `mgrbyte-claude-dep-name-prefixes'."
  (cl-some (lambda (prefix) (string-prefix-p prefix name))
           mgrbyte-claude-dep-name-prefixes))

(defun mgrbyte-claude-read-pyproject-deps (pyproject-path)
  "Extract local dependency names from pyproject.toml at PYPROJECT-PATH.
Returns a list of dependency names matching `mgrbyte-claude-dep-name-prefixes'."
  (when (file-exists-p pyproject-path)
    (let* ((data (toml:read-from-file pyproject-path))
           (project (cdr (assoc "project" data)))
           (deps (when project (cdr (assoc "dependencies" project)))))
      (when deps
        (cl-remove-if-not
         #'mgrbyte-claude--matches-prefix-p
         (mapcar #'mgrbyte-claude--strip-version-spec
                 (append deps nil)))))))

(defun mgrbyte-claude-resolve-dep-path (dep-name locations)
  "Find DEP-NAME in LOCATIONS.  Return the first matching directory or nil."
  (cl-some (lambda (location)
             (let ((path (expand-file-name dep-name (expand-file-name location))))
               (when (file-directory-p path) path)))
           locations))

(defun mgrbyte-claude-resolve-deps-recursive (pyproject-path locations depth &optional seen)
  "Resolve local deps from PYPROJECT-PATH, recursing up to DEPTH.
LOCATIONS is a list of parent directories to search.
SEEN is an internal accumulator of already-resolved directory paths.
Returns a list of unique directory paths."
  (let ((seen (or seen (list)))
        (dep-names (mgrbyte-claude-read-pyproject-deps pyproject-path)))
    (dolist (name dep-names)
      (let ((dep-dir (mgrbyte-claude-resolve-dep-path name locations)))
        (when (and dep-dir (not (member dep-dir seen)))
          (push dep-dir seen)
          (when (> depth 1)
            (let ((sub-pyproject (expand-file-name "pyproject.toml" dep-dir)))
              (when (file-exists-p sub-pyproject)
                (setq seen (mgrbyte-claude-resolve-deps-recursive
                            sub-pyproject locations (1- depth) seen))))))))
    seen))

(defun mgrbyte-claude--build-add-dir-flags (dirs)
  "Build a CLI flags string from DIRS for --add-dir."
  (mapconcat (lambda (dir) (concat "--add-dir " (shell-quote-argument dir)))
             dirs " "))

(defun mgrbyte-claude--resolve-add-dir-dirs (working-dir)
  "Resolve add-dir directories for WORKING-DIR from its .dir-locals.el.
Returns a list of directory paths, or nil if no deps found."
  (let* ((dir-locals-file (expand-file-name ".dir-locals.el" working-dir))
         (dir-locals (mgrbyte-claude--read-dir-locals dir-locals-file))
         (nil-alist (cdr (assq nil dir-locals)))
         (locations (cdr (assq 'mgrbyte-claude-library-locations nil-alist)))
         (pyproject-rel (or (cdr (assq 'mgrbyte-claude-pyproject-path nil-alist))
                            "pyproject.toml")))
    (when locations
      (let* ((pyproject-abs (expand-file-name pyproject-rel working-dir))
             (dep-dirs (mgrbyte-claude-resolve-deps-recursive
                        pyproject-abs locations
                        mgrbyte-claude-max-dep-depth)))
        (when dep-dirs
          (message "mgrbyte-claude: resolved %d add-dir dependencies"
                   (length dep-dirs))
          dep-dirs)))))

(defvar mgrbyte-claude--project-markers '(".git" "pyproject.toml" "flake.nix"
                                          "package.json" "Cargo.toml" "go.mod")
  "Files that indicate a directory is a project root (stop recursing).")

(defun mgrbyte-claude--project-root-p (dir)
  "Return non-nil if DIR contains a project marker file."
  (cl-some (lambda (marker) (file-exists-p (expand-file-name marker dir)))
           mgrbyte-claude--project-markers))

(defun mgrbyte-claude--subdirs (dir depth)
  "Return non-project subdirectories of DIR up to DEPTH levels.
Stops recursing into directories that are project roots."
  (when (and (> depth 0) (file-directory-p dir))
    (let ((subdirs (cl-remove-if-not
                    #'file-directory-p
                    (directory-files dir t "^[^.]"))))
      (cl-loop for sub in subdirs
               unless (mgrbyte-claude--project-root-p sub)
               collect sub
               and append (mgrbyte-claude--subdirs sub (1- depth))))))

(defun mgrbyte-claude--read-dir-locals (dir-locals-file)
  "Read existing .dir-locals.el from DIR-LOCALS-FILE, or nil."
  (when (file-exists-p dir-locals-file)
    (with-temp-buffer
      (insert-file-contents dir-locals-file)
      (read (current-buffer)))))

(defun mgrbyte-claude--write-dir-locals (dir-locals-file nil-alist)
  "Write NIL-ALIST as the nil mode entry in DIR-LOCALS-FILE.
Preserves other mode entries if present."
  (let* ((existing (mgrbyte-claude--read-dir-locals dir-locals-file))
         (new-locals (cons (cons nil nil-alist)
                           (assq-delete-all nil existing))))
    (with-temp-file dir-locals-file
      (pp new-locals (current-buffer)))))

(defun mgrbyte-claude-setup-library-locations ()
  "Select library parent directories for Claude --add-dir resolution.
Presents projectile known projects in a helm multi-select buffer.
Writes `mgrbyte-claude-library-locations' to .dir-locals.el."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (unless project-root
      (user-error "Not in a project"))
    (let* ((candidates
            (delete-dups
             (cl-loop
              for entry in projectile-project-search-path
              for root = (expand-file-name (if (consp entry) (car entry) entry))
              for depth = (if (consp entry) (cdr entry) 1)
              collect root
              append (mgrbyte-claude--subdirs root depth))))
           (selected (helm-comp-read
                      "Library parent directories (mark with C-SPC): "
                      (sort candidates #'string<)
                      :marked-candidates t))
           (locations (mapcar #'directory-file-name
                              (if (listp selected) selected (list selected))))
           (dir-locals-file (expand-file-name ".dir-locals.el" project-root))
           (existing (mgrbyte-claude--read-dir-locals dir-locals-file))
           (nil-alist (or (cdr (assq nil existing)) '()))
           (nil-alist (cons (cons 'mgrbyte-claude-library-locations locations)
                            (assq-delete-all 'mgrbyte-claude-library-locations nil-alist))))
      (mgrbyte-claude--write-dir-locals dir-locals-file nil-alist)
      (hack-dir-local-variables-non-file-buffer)
      (message "mgrbyte-claude: wrote library locations to %s" dir-locals-file))))

(defun mgrbyte-claude-setup-pyproject-path ()
  "Set the relative path to pyproject.toml for the current project.
Only needed when pyproject.toml is not at the project root.
Writes `mgrbyte-claude-pyproject-path' to .dir-locals.el."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (unless project-root
      (user-error "Not in a project"))
    (let* ((pyproject-at-root (file-exists-p
                               (expand-file-name "pyproject.toml" project-root)))
           (pyproject-files
            (unless pyproject-at-root
              (let ((results '()))
                (dolist (f (directory-files project-root t "^[^.]" t))
                  (when (file-directory-p f)
                    (let ((candidate (expand-file-name "pyproject.toml" f)))
                      (when (file-exists-p candidate)
                        (push (file-relative-name candidate project-root) results)))))
                (nreverse results))))
           (pyproject-path
            (cond
             (pyproject-at-root
              (message "pyproject.toml found at project root, no override needed") nil)
             ((null pyproject-files)
              (message "No pyproject.toml found in project") nil)
             ((= (length pyproject-files) 1)
              (car pyproject-files))
             (t (helm-comp-read "Select pyproject.toml: " pyproject-files)))))
      (when pyproject-path
        (let* ((dir-locals-file (expand-file-name ".dir-locals.el" project-root))
               (existing (mgrbyte-claude--read-dir-locals dir-locals-file))
               (nil-alist (or (cdr (assq nil existing)) '()))
               (nil-alist (cons (cons 'mgrbyte-claude-pyproject-path pyproject-path)
                                (assq-delete-all 'mgrbyte-claude-pyproject-path nil-alist))))
          (mgrbyte-claude--write-dir-locals dir-locals-file nil-alist)
          (hack-dir-local-variables-non-file-buffer)
          (message "mgrbyte-claude: wrote pyproject path '%s' to %s" pyproject-path dir-locals-file))))))

(provide 'mgrbyte-claude)
;;; mgrbyte-claude.el ends here
