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
;;
;; Issue selection (no milestones — reporting periods are dated sub-epics
;; of a cycle epic):
;; - "periods" are the dated children of the cycle epic
;; - an epic is in play when its ancestor chain reaches a period whose
;;   span contains today and the chain carries the configured label
;; - open issues under in-play epics are kept when assigned to the
;;   configured assignee or carrying the configured label
;; - epic-less open issues are kept when assigned to the configured
;;   assignee
;; - issues labelled blocked/postponed are dropped
;;; Code:

(require 'json)
(require 'org)
(require 'seq)

(defvar mgrbyte-gitlab-sync-hostname nil
  "GitLab hostname for glab API calls.")

(defvar mgrbyte-gitlab-sync-group-id nil
  "GitLab group ID for the epic source group.")

(defvar mgrbyte-gitlab-sync-issues-group-id nil
  "GitLab group ID for the issue source group.")

(defvar mgrbyte-gitlab-sync-label nil
  "Label a chain must carry for its epics and issues to be in play.")

(defvar mgrbyte-gitlab-sync-cycle-epic-id nil
  "Epic ID of the cycle epic whose dated children are reporting periods.")

(defvar mgrbyte-gitlab-sync-assignee nil
  "GitLab username whose assigned issues are always kept.")

(defvar mgrbyte-gitlab-sync-drop-labels '("Wedi'i rwystro" "Wedi'i ohirio")
  "Issues carrying any of these labels are dropped (blocked/postponed).")

(defvar mgrbyte-gitlab-sync-output-file nil
  "Path to the generated org file.")

(defvar mgrbyte-gitlab-sync-org-title nil
  "Title for the generated org file.")

(defvar mgrbyte-gitlab-sync--excluded-labels nil
  "Labels excluded from org tags (time periods and status indicators).")

;; Load private configuration (deployed via nix-secrets)
(require 'init-gitlab-sync-config nil t)

(defun mgrbyte-gitlab-sync--configured-p ()
  "Return non-nil if all required sync settings are present."
  (and mgrbyte-gitlab-sync-hostname
       mgrbyte-gitlab-sync-group-id
       mgrbyte-gitlab-sync-issues-group-id
       mgrbyte-gitlab-sync-label
       mgrbyte-gitlab-sync-cycle-epic-id
       mgrbyte-gitlab-sync-assignee
       mgrbyte-gitlab-sync-output-file))

(defun mgrbyte-gitlab-sync--field (key alist)
  "Return KEY's value in ALIST, nil when absent or JSON null."
  (let ((val (alist-get key alist)))
    (unless (eq val :null) val)))

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

(defun mgrbyte-gitlab-sync--glab-api-paged (endpoint)
  "Fetch every page of ENDPOINT (which must already contain a query string)."
  (let ((page 1)
        (results nil)
        (done nil))
    (while (not done)
      (let ((batch (mgrbyte-gitlab-sync--glab-api
                    (format "%s&per_page=100&page=%d" endpoint page))))
        (setq results (nconc results batch))
        (if (< (length batch) 100)
            (setq done t)
          (setq page (1+ page)))))
    results))

(defun mgrbyte-gitlab-sync--fetch-epics ()
  "Fetch all open epics from the parent group."
  (mgrbyte-gitlab-sync--glab-api-paged
   (format "groups/%d/epics?state=opened"
           mgrbyte-gitlab-sync-group-id)))

(defun mgrbyte-gitlab-sync--fetch-issues ()
  "Fetch all open issues from the configured group.
`issue_type=issue' excludes task work items, which share the project
iid space and would otherwise appear in the list."
  (mgrbyte-gitlab-sync--glab-api-paged
   (format "groups/%d/issues?state=opened&include_subgroups=true&issue_type=issue"
           mgrbyte-gitlab-sync-issues-group-id)))

;;; --- Filtering and grouping ---

(defun mgrbyte-gitlab-sync--build-epic-map (epics)
  "Build alist mapping epic ID to epic data from EPICS.
Used to walk parent chains and look up epic info for issues."
  (mapcar (lambda (epic)
            (cons (alist-get 'id epic) epic))
          epics))

(defun mgrbyte-gitlab-sync--period-epic-p (epic)
  "Return non-nil when EPIC is a dated child of the cycle epic."
  (and (equal (mgrbyte-gitlab-sync--field 'parent_id epic)
              mgrbyte-gitlab-sync-cycle-epic-id)
       (mgrbyte-gitlab-sync--field 'start_date epic)
       (mgrbyte-gitlab-sync--field 'due_date epic)
       t))

(defun mgrbyte-gitlab-sync--active-period-p (epic)
  "Return non-nil when EPIC is a period whose span contains today."
  (and (mgrbyte-gitlab-sync--period-epic-p epic)
       (let ((today (format-time-string "%Y-%m-%d")))
         ;; ISO dates compare lexicographically; string<= does not exist.
         (and (not (string> (mgrbyte-gitlab-sync--field 'start_date epic) today))
              (not (string> today (mgrbyte-gitlab-sync--field 'due_date epic)))))))

(defun mgrbyte-gitlab-sync--epic-chain (epic epic-map)
  "Return EPIC followed by its ancestors, walking parent links in EPIC-MAP."
  (let ((chain nil)
        (seen nil)
        (current epic))
    (while (and current (not (memql (alist-get 'id current) seen)))
      (push (alist-get 'id current) seen)
      (push current chain)
      (setq current
            (when-let ((parent-id (mgrbyte-gitlab-sync--field 'parent_id current)))
              (alist-get parent-id epic-map))))
    (nreverse chain)))

(defun mgrbyte-gitlab-sync--epic-in-play-p (epic epic-map)
  "Return non-nil when EPIC belongs to the current reporting period.
The ancestor chain must reach a period whose span contains today, and
the chain (EPIC itself or an ancestor) must carry the configured label."
  (let ((chain (mgrbyte-gitlab-sync--epic-chain epic epic-map)))
    (and (seq-some #'mgrbyte-gitlab-sync--active-period-p chain)
         (seq-some (lambda (e)
                     (seq-contains-p (alist-get 'labels e)
                                     mgrbyte-gitlab-sync-label #'string=))
                   chain))))

(defun mgrbyte-gitlab-sync--filter-epics (epics epic-map)
  "Filter EPICS to in-play deliverables using EPIC-MAP.
The cycle epic and the period epics themselves are structural and
excluded from the output."
  (seq-filter
   (lambda (epic)
     (and (not (equal (alist-get 'id epic) mgrbyte-gitlab-sync-cycle-epic-id))
          (not (mgrbyte-gitlab-sync--period-epic-p epic))
          (mgrbyte-gitlab-sync--epic-in-play-p epic epic-map)))
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
  (when-let ((epic-data (mgrbyte-gitlab-sync--field 'epic issue)))
    (when (listp epic-data)
      (alist-get (alist-get 'id epic-data) epic-map))))

(defun mgrbyte-gitlab-sync--issue-assigned-p (issue)
  "Return non-nil when ISSUE is assigned to the configured assignee."
  (seq-some (lambda (assignee)
              (string= (alist-get 'username assignee)
                       mgrbyte-gitlab-sync-assignee))
            (mgrbyte-gitlab-sync--field 'assignees issue)))

(defun mgrbyte-gitlab-sync--issue-labelled-p (issue)
  "Return non-nil when ISSUE carries the configured label."
  (seq-contains-p (mgrbyte-gitlab-sync--field 'labels issue)
                  mgrbyte-gitlab-sync-label #'string=))

(defun mgrbyte-gitlab-sync--issue-dropped-p (issue)
  "Return non-nil when ISSUE carries a label in `mgrbyte-gitlab-sync-drop-labels'."
  (let ((labels (mgrbyte-gitlab-sync--field 'labels issue)))
    (seq-some (lambda (drop) (seq-contains-p labels drop #'string=))
              mgrbyte-gitlab-sync-drop-labels)))

(defun mgrbyte-gitlab-sync--issue-keep-p (issue in-play-ids)
  "Return non-nil when ISSUE belongs on the dashboard.
Epic-attached issues must sit under an epic in IN-PLAY-IDS and be
assigned to the configured assignee or carry the configured label.
Epic-less issues must be assigned to the configured assignee."
  (and (not (mgrbyte-gitlab-sync--issue-dropped-p issue))
       (let ((epic-data (mgrbyte-gitlab-sync--field 'epic issue)))
         (if (and epic-data (listp epic-data))
             (and (memql (alist-get 'id epic-data) in-play-ids)
                  (or (mgrbyte-gitlab-sync--issue-assigned-p issue)
                      (mgrbyte-gitlab-sync--issue-labelled-p issue)))
           (mgrbyte-gitlab-sync--issue-assigned-p issue)))))

(defun mgrbyte-gitlab-sync--filter-issues (issues in-play-ids)
  "Filter ISSUES to those kept for the dashboard given IN-PLAY-IDS."
  (seq-filter (lambda (issue)
                (mgrbyte-gitlab-sync--issue-keep-p issue in-play-ids))
              issues))

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
                          (lambda (i)
                            (or (mgrbyte-gitlab-sync--field 'due_date i) "9999"))
                          #'string< issues))
            (let* ((title (alist-get 'title issue))
                   (state (alist-get 'state issue))
                   (todo-state (pcase state
                                 ("opened" "TODO")
                                 ("closed" "DONE")
                                 (_ "TODO")))
                   (due-date (alist-get 'due_date issue))
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
    ;; Split so the marker text is not contiguous in this source file,
    ;; which Emacs would otherwise misparse as its local-variables list.
    (insert "\n# Local " "Variables:\n")
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
  (if (not (mgrbyte-gitlab-sync--configured-p))
      (message "GitLab sync skipped: init-gitlab-sync-config not available")
    (message "Syncing GitLab epics and issues...")
    (condition-case err
      (let* ((all-epics (mgrbyte-gitlab-sync--fetch-epics))
             (epic-map (mgrbyte-gitlab-sync--build-epic-map all-epics))
             (in-play-epics (mgrbyte-gitlab-sync--filter-epics all-epics epic-map))
             (in-play-ids (mapcar (lambda (e) (alist-get 'id e)) in-play-epics))
             (all-issues (mgrbyte-gitlab-sync--fetch-issues))
             (kept-issues (mgrbyte-gitlab-sync--filter-issues all-issues in-play-ids))
             (grouped-issues (mgrbyte-gitlab-sync--group-issues-by-project kept-issues))
             (content (mgrbyte-gitlab-sync--generate-org
                       in-play-epics grouped-issues epic-map)))
        (mgrbyte-gitlab-sync--write-org-file content)
        (message "GitLab sync complete: %d epics, %d issues across %d projects"
                 (length in-play-epics)
                 (length kept-issues)
                 (length grouped-issues)))
      (error (message "GitLab sync failed: %s" (error-message-string err))))))

(defun mgrbyte-gitlab-sync-async ()
  "Run `mgrbyte-gitlab-sync' without blocking."
  (run-with-idle-timer 1 nil #'mgrbyte-gitlab-sync))

(global-set-key (kbd "C-c g s") #'mgrbyte-gitlab-sync)

(with-eval-after-load 'dashboard
  (add-hook 'dashboard-after-initialize-hook #'mgrbyte-gitlab-sync-async))

(provide 'init-gitlab-sync)
;;; init-gitlab-sync.el ends here
