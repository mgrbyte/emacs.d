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
         (marker (epicworkmgr--blocked-marker
                  (length (alist-get 'blocked_by issue))))
         (title-str (if (alist-get 'active issue)
                        (propertize title 'face 'epicworkmgr-active)
                      title)))
    (concat (epicworkmgr--priority-icon (alist-get 'priority issue)) " "
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

(provide 'init-epicworkmgr)
;;; init-epicworkmgr.el ends here
