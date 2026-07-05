;;; init-epicworkmgr.el --- Render techiaith-epicworkmgr agenda -*- lexical-binding: t -*-
;;; Commentary:
;; A magit-section buffer, *techiaith-gitlab*, rendering the nested JSON emitted
;; by the `techiaith-epicworkmgr agenda' CLI: epic groups -> issues -> blockers,
;; plus an "Ungrouped" peer section.  This module wraps an external CLI (analogous
;; to claude-code.el wrapping the claude-code CLI), hence the bare `epicworkmgr-'
;; package prefix rather than the personal-config `mgrbyte-' prefix.
;;
;; Data is fetched asynchronously (the CLI takes several seconds); a header-line
;; indicator shows fetching / refreshing / synced-at state.  Ordering is taken
;; verbatim from the JSON (the CLI already ranks and orders) -- this renderer
;; never re-sorts.
;;; Code:

(require 'iso8601)
(require 'magit-section)
(require 'seq)
(require 'subr-x)
(require 'projectile)

;;; --- Faces ---

(defgroup epicworkmgr nil
  "Render the techiaith-epicworkmgr agenda."
  :group 'tools)

(defface epicworkmgr-priority-a '((t :inherit error))
  "Face for the priority-A icon."
  :group 'epicworkmgr)

(defface epicworkmgr-priority-b '((t :inherit warning))
  "Face for the priority-B icon."
  :group 'epicworkmgr)

(defface epicworkmgr-priority-c '((t :inherit shadow))
  "Face for the priority-C icon."
  :group 'epicworkmgr)

(defface epicworkmgr-active '((t :inherit success :weight bold))
  "Face for an active (in-progress) issue."
  :group 'epicworkmgr)

(defface epicworkmgr-blocked '((t :inherit error))
  "Face for the blocked icon and blocker count."
  :group 'epicworkmgr)

(defface epicworkmgr-epic '((t :inherit magit-section-heading))
  "Face for epic-group and Ungrouped section headers."
  :group 'epicworkmgr)

;;; --- Parsing & pure helpers ---

(defun epicworkmgr--parse (json-string)
  "Parse JSON-STRING (an agenda snapshot) into an alist.
Objects become alists, arrays lists; JSON null -> nil, false -> nil."
  (json-parse-string json-string
                     :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun epicworkmgr--host-from-snapshot (snapshot)
  "Return the scheme+host (e.g. \"https://host\") from any issue in SNAPSHOT.
Blockers carry no web_url, so their URLs are rebuilt against this host."
  (let* ((groups (alist-get 'epics snapshot))
         (issue (or (car (alist-get 'issues (car groups)))
                    (car (alist-get 'ungrouped snapshot))))
         (url (alist-get 'web_url issue)))
    (when (and url (string-match "\\`\\(https?://[^/]+\\)" url))
      (match-string 1 url))))

(defun epicworkmgr--blocker-url (ref host)
  "Build a work-items URL for blocker REF (\"group/../project#iid\") under HOST."
  (let ((parts (split-string ref "#")))
    (format "%s/%s/-/work_items/%s" host (car parts) (cadr parts))))

(defun epicworkmgr--format-generated-at (iso &optional zone)
  "Format ISO 8601 timestamp ISO as \"HH:MM\".  ZONE as in `format-time-string'."
  (format-time-string "%H:%M" (encode-time (iso8601-parse iso)) zone))

;;; --- Icons & issue line ---

(defconst epicworkmgr--priority-icons
  '(("A" #xf2c7 epicworkmgr-priority-a)
    ("B" #xf2c9 epicworkmgr-priority-b)
    ("C" #xf2cb epicworkmgr-priority-c))
  "Alist of priority letter -> (codepoint face).")

(defconst epicworkmgr--blocked-icon #xf256
  "Nerd-font codepoint for the blocked marker.")

(defconst epicworkmgr--active-icon #xeb79
  "Nerd-font codepoint for the active (in-progress) marker.")

(defun epicworkmgr--active-marker ()
  "Return the faced active-marker icon string."
  (propertize (char-to-string epicworkmgr--active-icon)
              'face 'epicworkmgr-active))

(defun epicworkmgr--priority-icon (priority)
  "Return a faced nerd-icon string for PRIORITY (\"A\"/\"B\"/\"C\"), or nil."
  (when-let ((spec (assoc priority epicworkmgr--priority-icons)))
    (propertize (char-to-string (nth 1 spec)) 'face (nth 2 spec))))

(defun epicworkmgr--blocked-marker (n)
  "Return a faced blocked-icon + count string for N blockers, \"\" when zero."
  (if (> n 0)
      (propertize (format "%c%d" epicworkmgr--blocked-icon n)
                  'face 'epicworkmgr-blocked)
    ""))

(defun epicworkmgr--issue-line (issue)
  "Return the propertized one-line rendering of ISSUE.
Priority icon, PROJECT#IID locator, title (active-faced when active), and a
trailing blocked marker when the issue has blockers."
  (let* ((title (alist-get 'title issue))
         (active (alist-get 'active issue))
         (marker (epicworkmgr--blocked-marker
                  (length (alist-get 'blocked_by issue))))
         (title-str (if active
                        (propertize title 'face 'epicworkmgr-active)
                      title)))
    (concat (when active (concat (epicworkmgr--active-marker) " "))
            (epicworkmgr--priority-icon (alist-get 'priority issue)) " "
            (format "%s#%s" (alist-get 'project issue) (alist-get 'iid issue))
            "  " title-str
            (unless (string-empty-p marker) (concat "  " marker)))))

;;; --- Project resolution ---

(defun epicworkmgr--project-root (name)
  "Return the known-project root whose directory basename is NAME, or nil.
Matches NAME against the basenames of `projectile-known-projects'."
  (seq-find (lambda (root)
              (equal (file-name-nondirectory (directory-file-name root)) name))
            projectile-known-projects))

;;; --- Header line (loading indicator) ---

(defun epicworkmgr--set-header (state &optional generated-at)
  "Set the buffer's header line for STATE, stamping GENERATED-AT where relevant.
STATE is `fetching', `refreshing', or `idle'."
  (setq-local header-line-format
              (pcase state
                ('fetching "⟳ Fetching agenda…")
                ('refreshing
                 (format "⟳ Refreshing… (showing %s)"
                         (epicworkmgr--format-generated-at generated-at)))
                ('idle
                 (format "techiaith agenda · synced %s · [g] refresh"
                         (epicworkmgr--format-generated-at generated-at))))))

;;; --- Rendering (magit-section) ---

(defun epicworkmgr--epic-header (epic)
  "Return the propertized header line for EPIC (active marker + faced title)."
  (concat (when (alist-get 'active epic)
            (concat (epicworkmgr--active-marker) " "))
          (propertize (alist-get 'title epic) 'face 'epicworkmgr-epic)))

(defun epicworkmgr--insert-blockers (blockers host)
  "Insert a foldable child section listing BLOCKERS.
Each blocker's section value is its work-items URL rebuilt under HOST."
  (magit-insert-section (epicworkmgr-blockers)
    (magit-insert-heading "blocked by:")
    (dolist (blocker blockers)
      (magit-insert-section
          (epicworkmgr-blocker
           (epicworkmgr--blocker-url (alist-get 'ref blocker) host))
        (insert (format "  %s#%s  %s\n"
                        (alist-get 'project blocker)
                        (alist-get 'iid blocker)
                        (alist-get 'title blocker)))))))

(defun epicworkmgr--insert-issue (issue host)
  "Insert ISSUE as a section; blocked issues get a foldable blockers child (HOST)."
  (magit-insert-section (epicworkmgr-issue issue)
    (magit-insert-heading (epicworkmgr--issue-line issue))
    (when-let ((blockers (alist-get 'blocked_by issue)))
      (epicworkmgr--insert-blockers blockers host))))

(defun epicworkmgr--insert-epic-group (group host)
  "Insert an epic GROUP section: epic header then its issues (HOST for blockers)."
  (let ((epic (alist-get 'epic group)))
    (magit-insert-section (epicworkmgr-epic epic)
      (magit-insert-heading (epicworkmgr--epic-header epic))
      (dolist (issue (alist-get 'issues group))
        (epicworkmgr--insert-issue issue host)))))

(defun epicworkmgr--insert-ungrouped (issues host)
  "Insert the Ungrouped peer section for epic-less ISSUES (HOST for blockers)."
  (magit-insert-section (epicworkmgr-ungrouped)
    (magit-insert-heading (propertize "Ungrouped" 'face 'epicworkmgr-epic))
    (dolist (issue issues)
      (epicworkmgr--insert-issue issue host))))

(defun epicworkmgr--render-snapshot (snapshot)
  "Render SNAPSHOT into the current buffer as a magit-section tree.
Order is taken verbatim from SNAPSHOT; this never re-sorts."
  (let ((host (epicworkmgr--host-from-snapshot snapshot))
        (inhibit-read-only t))
    (erase-buffer)
    (magit-insert-section (epicworkmgr-root)
      (dolist (group (alist-get 'epics snapshot))
        (epicworkmgr--insert-epic-group group host))
      (when-let ((ungrouped (alist-get 'ungrouped snapshot)))
        (epicworkmgr--insert-ungrouped ungrouped host)))))

;;; --- Interaction ---

(defun epicworkmgr--section-target-url (value)
  "Return the URL a section carrying VALUE should visit, or nil.
Blocker sections store a URL string; issue/epic sections store an alist."
  (cond ((stringp value) value)
        ((consp value) (alist-get 'web_url value))
        (t nil)))

(defun epicworkmgr-visit ()
  "Visit the GitLab URL of the section at point in a browser."
  (interactive)
  (when-let* ((section (magit-current-section))
              (url (epicworkmgr--section-target-url (oref section value))))
    (browse-url url)))

(defun epicworkmgr--switch-project-other-window (root)
  "Open the projectile project at ROOT in another window."
  (let ((display-buffer-overriding-action
         '((display-buffer-use-some-window) (inhibit-same-window . t))))
    (projectile-switch-project-by-name root)))

(defun epicworkmgr-jump-to-project ()
  "Switch to the local checkout of the project for the issue at point."
  (interactive)
  (let* ((section (magit-current-section))
         (value (and section (oref section value)))
         (project (and (consp value) (alist-get 'project value)))
         (root (and project (epicworkmgr--project-root project))))
    (cond ((null project) (message "No issue project at point"))
          ((null root) (message "%s: no local checkout" project))
          (t (epicworkmgr--switch-project-other-window root)))))

;;; --- Async fetch & commands ---

(defvar-local epicworkmgr--process nil
  "The in-flight agenda fetch process for this buffer, if any.")

(defvar-local epicworkmgr--generated-at nil
  "`generated_at' of the snapshot currently shown in this buffer.")

(defun epicworkmgr--render-error (stderr)
  "Render a fetch failure carrying STDERR into the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Failed to fetch the agenda:\n\n" stderr))
  (setq-local header-line-format "⚠ agenda fetch failed · [g] retry"))

(defun epicworkmgr--fetch (on-success)
  "Fetch the agenda asynchronously and call ON-SUCCESS with the parsed snapshot.
Guards against concurrent fetches; renders an error on non-zero exit."
  (when (process-live-p epicworkmgr--process)
    (user-error "An agenda fetch is already in progress"))
  (let ((buffer (current-buffer))
        (output (generate-new-buffer " *epicworkmgr-output*")))
    (setq epicworkmgr--process
          (make-process
           :name "epicworkmgr-agenda"
           :buffer output
           :noquery t
           :command '("techiaith-epicworkmgr" "agenda")
           :sentinel
           (lambda (proc _event)
             (unless (process-live-p proc)
               (unwind-protect
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (if (zerop (process-exit-status proc))
                           (funcall on-success
                                    (epicworkmgr--parse
                                     (with-current-buffer output (buffer-string))))
                         (epicworkmgr--render-error
                          (with-current-buffer output (buffer-string))))))
                 (kill-buffer output))))))))

(defun epicworkmgr--load (state)
  "Set the header to STATE, then fetch and render the agenda on success."
  (epicworkmgr--set-header state epicworkmgr--generated-at)
  (epicworkmgr--fetch
   (lambda (snapshot)
     (epicworkmgr--render-snapshot snapshot)
     (setq-local epicworkmgr--generated-at (alist-get 'generated_at snapshot))
     (epicworkmgr--set-header 'idle epicworkmgr--generated-at))))

(defun epicworkmgr-refresh ()
  "Re-fetch the agenda, keeping the current content visible while it loads."
  (interactive)
  (epicworkmgr--load 'refreshing))

;;; --- Mode ---

(defvar epicworkmgr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'epicworkmgr-visit)
    (define-key map [mouse-1] #'epicworkmgr-visit)
    (define-key map (kbd "g") #'epicworkmgr-refresh)
    (define-key map (kbd "o") #'epicworkmgr-jump-to-project)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `epicworkmgr-mode'.")

(define-derived-mode epicworkmgr-mode magit-section-mode "Epicworkmgr"
  "Major mode rendering the techiaith-epicworkmgr agenda."
  :group 'epicworkmgr)

;;;###autoload
(defun epicworkmgr ()
  "Open the techiaith epicworkmgr agenda buffer and fetch it."
  (interactive)
  (let ((buffer (get-buffer-create "*techiaith-gitlab*")))
    (with-current-buffer buffer
      (unless (derived-mode-p 'epicworkmgr-mode) (epicworkmgr-mode))
      (epicworkmgr--load 'fetching))
    (pop-to-buffer buffer)))

(global-set-key (kbd "C-c g a") #'epicworkmgr)

(provide 'init-epicworkmgr)
;;; init-epicworkmgr.el ends here
