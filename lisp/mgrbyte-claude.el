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
    map)
  "Keymap for `mgrbyte-claude-mode'.")

;;;###autoload
(define-minor-mode mgrbyte-claude-mode
  "Toggle mgrbyte-claude mode.

\\{mgrbyte-claude-keymap}
Provides Claude Code IDE integration using an external terminal
for rendering with Emacs handling IDE features via MCP."
  :lighter " Claude"
  :global t
  :keymap mgrbyte-claude-keymap
  :group 'mgrbyte)

;;;; External terminal integration

(defvar mgrbyte-claude-ide-external-script "claude-ide-external"
  "Name of the external launcher script (must be on PATH).")

(defvar mgrbyte-claude-ide-external--sessions (make-hash-table :test 'equal)
  "Hash table mapping working directories to external session plists.
Each plist contains :port :session-id :window-name :working-dir.")

(defun mgrbyte-claude-ide-external--cleanup (working-dir)
  "Clean up session state for WORKING-DIR."
  (let ((session (gethash working-dir mgrbyte-claude-ide-external--sessions)))
    (when session
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
           (script-args (list mgrbyte-claude-ide-external-script
                              "--port" (number-to-string port)
                              "--session-id" session-id
                              "--mcp-url" (or mcp-url "")
                              "--working-dir" working-dir)))
      (when resume (setq script-args (append script-args (list "--resume"))))
      (when continue (setq script-args (append script-args (list "--continue"))))
      (when allowed-tools
        (setq script-args (append script-args (list "--allowed-tools" allowed-tools))))
      (when (and claude-code-ide-cli-extra-flags
                 (not (string-empty-p claude-code-ide-cli-extra-flags)))
        (setq script-args (append script-args (list "--extra-flags" claude-code-ide-cli-extra-flags))))
      ;; Kill any stale tmux window
      (ignore-errors
        (call-process "tmux" nil nil nil "kill-window" "-t" window-name))
      ;; Launch in tmux — pass script and args directly (no shell quoting)
      (apply #'call-process "tmux" nil nil nil
             "new-window" "-n" window-name script-args)
      ;; Notify MCP tools server about the session
      (claude-code-ide-mcp-server-session-started session-id working-dir nil)
      ;; Track the session
      (puthash working-dir
               (list :port port :session-id session-id
                     :window-name window-name :working-dir working-dir)
               mgrbyte-claude-ide-external--sessions)
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
   ("R" "Resume in directory" mgrbyte-claude-ide-external-resume-in-directory)])

;;;; Auto-dependency discovery (requires toml and projectile)

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
           (project (gethash "project" data))
           (deps (when project (gethash "dependencies" project))))
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

(defun mgrbyte-claude-configure-add-dirs ()
  "Configure `claude-code-ide-cli-extra-flags' with --add-dir for local deps.
Intended for use on `projectile-after-switch-project-hook'.
Reads `mgrbyte-claude-library-locations' and `mgrbyte-claude-pyproject-path'
from dir-local variables."
  (when mgrbyte-claude-library-locations
    (let* ((project-root (projectile-project-root))
           (pyproject-rel (or mgrbyte-claude-pyproject-path "pyproject.toml"))
           (pyproject-abs (expand-file-name pyproject-rel project-root))
           (dep-dirs (mgrbyte-claude-resolve-deps-recursive
                      pyproject-abs
                      mgrbyte-claude-library-locations
                      mgrbyte-claude-max-dep-depth)))
      (when dep-dirs
        (setq claude-code-ide-cli-extra-flags
              (mgrbyte-claude--build-add-dir-flags dep-dirs))
        (message "mgrbyte-claude: configured --add-dir for %d dependencies"
                 (length dep-dirs))))))

(defun mgrbyte-claude-setup-dir-locals ()
  "Configure .dir-locals.el for Claude dependency resolution.
Prompts for library parent directories from `projectile-known-projects'
and optionally a custom pyproject.toml path.  Writes or updates
.dir-locals.el in the current project root."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (unless project-root
      (user-error "Not in a project"))
    (let* ((known-projects (projectile-relevant-known-projects))
           (selected (completing-read-multiple
                      "Library parent directories: "
                      known-projects))
           (locations (mapcar #'directory-file-name selected))
           (pyproject-at-root (file-exists-p
                               (expand-file-name "pyproject.toml" project-root)))
           (pyproject-path (unless pyproject-at-root
                             (read-string "Relative path to pyproject.toml (e.g. backend/pyproject.toml): ")))
           (dir-locals-file (expand-file-name ".dir-locals.el" project-root))
           (existing (when (file-exists-p dir-locals-file)
                       (with-temp-buffer
                         (insert-file-contents dir-locals-file)
                         (read (current-buffer)))))
           (nil-alist (or (cdr (assq nil existing)) '()))
           (nil-alist (cons (cons 'mgrbyte-claude-library-locations locations)
                            (assq-delete-all 'mgrbyte-claude-library-locations nil-alist))))
      (when (and pyproject-path (not (string-empty-p pyproject-path)))
        (setq nil-alist (cons (cons 'mgrbyte-claude-pyproject-path pyproject-path)
                              (assq-delete-all 'mgrbyte-claude-pyproject-path nil-alist))))
      (let ((new-locals (cons (cons nil nil-alist)
                              (assq-delete-all nil existing))))
        (with-temp-file dir-locals-file
          (pp new-locals (current-buffer))))
      (hack-dir-local-variables-non-file-buffer)
      (message "mgrbyte-claude: wrote %s" dir-locals-file))))

(provide 'mgrbyte-claude)
;;; mgrbyte-claude.el ends here
