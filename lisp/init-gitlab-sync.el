;;; init-gitlab-sync.el --- GitLab epic/issue sync to org-agenda -*- lexical-binding: t -*-
;;; Commentary:
;; Syncs GitLab epics and issues to a read-only org file for display in
;; org-agenda and the emacs dashboard.
;;
;; Requires init-gitlab-sync-config.el (not version controlled here) to
;; provide instance-specific settings: hostname, group IDs, labels, etc.
;; That file is deployed via nix-secrets on configured machines.
;;
;; Generated file has two sections:
;; 1. Epic overview (reference, no TODO states, doesn't appear in agenda)
;; 2. Issues grouped by project (actionable TODOs for the agenda)
;;; Code:

(require 'json)
(require 'org)
(require 'url-util)

(defvar mgrbyte-gitlab-sync-hostname nil
  "GitLab hostname for glab API calls.")

(defvar mgrbyte-gitlab-sync-group-id nil
  "GitLab group ID for the epic source group.")

(defvar mgrbyte-gitlab-sync-issues-group-id nil
  "GitLab group ID for the issue source group.")

(defvar mgrbyte-gitlab-sync-label nil
  "Label to filter epics by.")

(defvar mgrbyte-gitlab-sync-milestone nil
  "Milestone to filter issues by.")

(defvar mgrbyte-gitlab-sync-output-file nil
  "Path to the generated org file.")

(defvar mgrbyte-gitlab-sync-org-title nil
  "Title for the generated org file.")

(defvar mgrbyte-gitlab-sync--container-epics nil
  "Alist mapping container epic IDs to display names.")

(defvar mgrbyte-gitlab-sync--container-epic-ids nil
  "List of container epic IDs to exclude from output.
Derived from `mgrbyte-gitlab-sync--container-epics'.")

(defvar mgrbyte-gitlab-sync--excluded-labels nil
  "Labels excluded from org tags (time periods and status indicators).")

;; Load private configuration (deployed via nix-secrets)
(require 'init-gitlab-sync-config nil t)

;; Derive container epic IDs after config is loaded
(when mgrbyte-gitlab-sync--container-epics
  (setq mgrbyte-gitlab-sync--container-epic-ids
        (mapcar #'car mgrbyte-gitlab-sync--container-epics)))

(defun mgrbyte-gitlab-sync--configured-p ()
  "Return non-nil if all required sync settings are present."
  (and mgrbyte-gitlab-sync-hostname
       mgrbyte-gitlab-sync-group-id
       mgrbyte-gitlab-sync-issues-group-id
       mgrbyte-gitlab-sync-label
       mgrbyte-gitlab-sync-milestone
       mgrbyte-gitlab-sync-output-file))

;;; --- API calls ---

(defun mgrbyte-gitlab-sync--glab-api (endpoint)
  "Call glab api ENDPOINT and return parsed JSON."
  (with-temp-buffer
    (let ((exit-code
           (call-process "glab" nil t nil
                         "api" endpoint
                         "--hostname" mgrbyte-gitlab-sync-hostname)))
      (unless (zerop exit-code)
        (error "glab api %s failed (exit %d): %s"
               endpoint exit-code (buffer-string)))
      (goto-char (point-min))
      (json-parse-buffer :object-type 'alist :array-type 'list))))

(defun mgrbyte-gitlab-sync--fetch-epics ()
  "Fetch all epics from the parent group."
  (mgrbyte-gitlab-sync--glab-api
   (format "groups/%d/epics?per_page=100&state=all"
           mgrbyte-gitlab-sync-group-id)))

(defun mgrbyte-gitlab-sync--fetch-issues ()
  "Fetch open issues from the configured group, filtered by milestone."
  (mgrbyte-gitlab-sync--glab-api
   (format "groups/%d/issues?per_page=100&state=opened&include_subgroups=true&milestone=%s"
           mgrbyte-gitlab-sync-issues-group-id
           (url-hexify-string mgrbyte-gitlab-sync-milestone))))

;;; --- Filtering and grouping ---

(defun mgrbyte-gitlab-sync--filter-epics (epics)
  "Filter EPICS to those with the configured label, excluding containers."
  (seq-filter
   (lambda (epic)
     (let ((labels (alist-get 'labels epic))
           (id (alist-get 'id epic)))
       (and (seq-contains-p labels mgrbyte-gitlab-sync-label #'string=)
            (not (memq id mgrbyte-gitlab-sync--container-epic-ids)))))
   epics))

(defun mgrbyte-gitlab-sync--build-epic-map (epics)
  "Build alist mapping epic IID to epic data from EPICS.
Used to look up epic info for issues."
  (mapcar (lambda (epic)
            (cons (alist-get 'iid epic) epic))
          epics))

(defun mgrbyte-gitlab-sync--issue-project-name (issue)
  "Extract short project name from ISSUE references."
  (let ((refs (alist-get 'references issue)))
    (when refs
      (let ((full (alist-get 'full refs)))
        (when (and full (string-match "/\\([^/]+\\)#[0-9]+$" full))
          (match-string 1 full))))))

(defun mgrbyte-gitlab-sync--issue-epic-info (issue epic-map)
  "Get epic info for ISSUE using EPIC-MAP.
Returns the epic alist or nil."
  (when-let ((epic-data (alist-get 'epic issue)))
    (when (listp epic-data)
      (let ((epic-iid (alist-get 'iid epic-data)))
        (alist-get epic-iid epic-map)))))

(defun mgrbyte-gitlab-sync--group-issues-by-project (issues)
  "Group ISSUES by project name.
Returns alist of (project-name . issues-list)."
  (let ((groups nil))
    (dolist (issue issues)
      (let ((project (or (mgrbyte-gitlab-sync--issue-project-name issue)
                         "unknown")))
        (if-let ((existing (assoc project groups)))
            (setcdr existing (append (cdr existing) (list issue)))
          (push (cons project (list issue)) groups))))
    (sort groups (lambda (a b) (string< (car a) (car b))))))

;;; --- Date formatting ---

(defun mgrbyte-gitlab-sync--format-date (date-str)
  "Format DATE-STR (YYYY-MM-DD) as org timestamp <YYYY-MM-DD Day>."
  (when (and date-str (stringp date-str))
    (let* ((parts (split-string date-str "-"))
           (year (string-to-number (nth 0 parts)))
           (month (string-to-number (nth 1 parts)))
           (day (string-to-number (nth 2 parts)))
           (time (encode-time 0 0 0 day month year))
           (day-name (format-time-string "%a" time)))
      (format "<%04d-%02d-%02d %s>" year month day day-name))))

(defun mgrbyte-gitlab-sync--format-datetime-as-date (datetime-str)
  "Extract date from DATETIME-STR (ISO 8601) and format as org timestamp."
  (when (and datetime-str (stringp datetime-str)
             (string-match "^\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" datetime-str))
    (mgrbyte-gitlab-sync--format-date (match-string 1 datetime-str))))

;;; --- Org tag helpers ---

(defun mgrbyte-gitlab-sync--label-to-tag (label)
  "Convert LABEL to a valid org tag (replace spaces with hyphens)."
  (replace-regexp-in-string "[[:space:]]+" "-" label))

(defun mgrbyte-gitlab-sync--epic-short-tag (epic)
  "Create a short tag from EPIC title, e.g. '208-Piblinellau'."
  (let ((title (alist-get 'title epic)))
    (when title
      (if (string-match "^\\([0-9]+\\)\\s-+\\(\\S-+\\)" title)
          (format "%s-%s" (match-string 1 title) (match-string 2 title))
        (mgrbyte-gitlab-sync--label-to-tag
         (truncate-string-to-width title 30))))))

;;; --- Org generation ---

(defun mgrbyte-gitlab-sync--generate-epic-overview (epics)
  "Generate the epic overview as a plain list (not headings).
Plain list items are invisible to org-agenda but links still work with C-c C-o."
  (with-temp-buffer
    (insert "Epics:\n")
    (dolist (epic (seq-sort-by
                   (lambda (e) (or (alist-get 'title e) ""))
                   #'string< epics))
      (let* ((title (alist-get 'title epic))
             (web-url (alist-get 'web_url epic)))
        (insert (format "- [[%s][%s]]\n" web-url title))))
    (buffer-string)))

(defun mgrbyte-gitlab-sync--generate-issues-section (grouped-issues epic-map)
  "Generate the issues-by-project section from GROUPED-ISSUES.
EPIC-MAP maps epic IID to epic data for tag generation."
  (with-temp-buffer
    (if (null grouped-issues)
        (insert "# No issues found.\n")
      (dolist (group grouped-issues)
        (let* ((project-name (car group))
               (issues (cdr group))
               (first-epic (mgrbyte-gitlab-sync--issue-epic-info (car issues) epic-map))
               (epic-tag (when first-epic
                           (mgrbyte-gitlab-sync--epic-short-tag first-epic))))
          (if epic-tag
              (insert (format "* %s %s\n" project-name
                              (format ":%s:" epic-tag)))
            (insert (format "* %s\n" project-name)))
          (dolist (issue (seq-sort-by
                          (lambda (i) (or (alist-get 'due_date i) "9999"))
                          #'string< issues))
            (let* ((title (alist-get 'title issue))
                   (state (alist-get 'state issue))
                   (todo-state (pcase state
                                 ("opened" "TODO")
                                 ("closed" "DONE")
                                 (_ "TODO")))
                   (due-date (alist-get 'due_date issue))
                   (created-at (alist-get 'created_at issue))
                   (web-url (alist-get 'web_url issue))
                   (iid (alist-get 'iid issue))
                   (epic (mgrbyte-gitlab-sync--issue-epic-info issue epic-map))
                   (epic-title (when epic (alist-get 'title epic)))
                   (epic-due (when epic (alist-get 'due_date epic))))
              (insert (format "** %s [#B] %s\n" todo-state title))
              (when due-date
                (let ((formatted (mgrbyte-gitlab-sync--format-date due-date)))
                  (when formatted
                    (insert (format "SCHEDULED: %s\n" formatted)))))
              (when epic-due
                (let ((formatted (mgrbyte-gitlab-sync--format-date epic-due)))
                  (when formatted
                    (insert (format "DEADLINE: %s\n" formatted)))))
              (insert ":PROPERTIES:\n")
              (insert (format ":GITLAB_URL: %s\n" web-url))
              (insert (format ":GITLAB_ISSUE_IID: %d\n" iid))
              (when epic-title
                (insert (format ":EPIC: %s\n" epic-title)))
              (insert ":END:\n\n"))))))
    (buffer-string)))

(defun mgrbyte-gitlab-sync--generate-org (filtered-epics grouped-issues epic-map)
  "Generate full org file from FILTERED-EPICS, GROUPED-ISSUES, and EPIC-MAP."
  (with-temp-buffer
    (insert (format "#+TITLE: %s\n" (or mgrbyte-gitlab-sync-org-title "GitLab Sync")))
    (insert "#+STARTUP: overview\n")
    (insert (format "# Auto-generated by mgrbyte-gitlab-sync. Do not edit manually.\n"))
    (insert (format "# Last synced: %s\n\n"
                    (format-time-string "%Y-%m-%d %H:%M")))
    (insert (mgrbyte-gitlab-sync--generate-epic-overview filtered-epics))
    (insert "\n")
    (insert (mgrbyte-gitlab-sync--generate-issues-section grouped-issues epic-map))
    (insert "\n# Local Variables:\n")
    (insert "# buffer-read-only: t\n")
    (insert "# End:\n")
    (buffer-string)))

;;; --- File writing ---

(defun mgrbyte-gitlab-sync--write-org-file (content)
  "Write CONTENT to the sync output file."
  (let ((file mgrbyte-gitlab-sync-output-file))
    (when (file-exists-p file)
      (set-file-modes file #o644))
    (with-temp-file file
      (insert content))
    (set-file-modes file #o444)
    (when-let ((buf (find-buffer-visiting file)))
      (with-current-buffer buf
        (revert-buffer t t t)))))

;;; --- Main entry points ---

;;;###autoload
(defun mgrbyte-gitlab-sync ()
  "Sync GitLab epics and issues to org-agenda file.
Requires init-gitlab-sync-config to provide instance-specific settings."
  (interactive)
  (unless (mgrbyte-gitlab-sync--configured-p)
    (message "GitLab sync skipped: init-gitlab-sync-config not available")
    (cl-return-from mgrbyte-gitlab-sync))
  (message "Syncing GitLab epics and issues...")
  (condition-case err
      (let* ((all-epics (mgrbyte-gitlab-sync--fetch-epics))
             (filtered-epics (mgrbyte-gitlab-sync--filter-epics all-epics))
             (epic-map (mgrbyte-gitlab-sync--build-epic-map all-epics))
             (all-issues (mgrbyte-gitlab-sync--fetch-issues))
             (grouped-issues (mgrbyte-gitlab-sync--group-issues-by-project all-issues))
             (content (mgrbyte-gitlab-sync--generate-org
                       filtered-epics grouped-issues epic-map)))
        (mgrbyte-gitlab-sync--write-org-file content)
        (message "GitLab sync complete: %d epics, %d issues across %d projects"
                 (length filtered-epics)
                 (length all-issues)
                 (length grouped-issues)))
    (error (message "GitLab sync failed: %s" (error-message-string err)))))

(defun mgrbyte-gitlab-sync-async ()
  "Run `mgrbyte-gitlab-sync' without blocking."
  (run-with-idle-timer 1 nil #'mgrbyte-gitlab-sync))

(global-set-key (kbd "C-c g s") #'mgrbyte-gitlab-sync)

(with-eval-after-load 'dashboard
  (add-hook 'dashboard-after-initialize-hook #'mgrbyte-gitlab-sync-async))

(provide 'init-gitlab-sync)
;;; init-gitlab-sync.el ends here
