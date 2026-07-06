;;; init-epicworkmgr-tests.el --- Tests for init-epicworkmgr -*- lexical-binding: t -*-
;;; Commentary:
;; ERT tests for the epicworkmgr agenda renderer.  Run in the live daemon via
;; the Emacs MCP: load-file the impl then this file, then select and run the
;; "epicworkmgr-" tests.  All fixture/test data is synthetic (no real hosts,
;; projects, or paths) so this file is safe in a public repository.
;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'init-epicworkmgr)

(defvar epicworkmgr-tests--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this test file, for locating fixtures.")

(defun epicworkmgr-tests--fixture (name)
  "Return the contents of fixture NAME under tests/fixtures/."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (concat "fixtures/" name) epicworkmgr-tests--dir))
    (buffer-string)))

;;; --- Cycle 1: parsing & pure helpers ---

(ert-deftest epicworkmgr-test-parse-reads-contract ()
  "`--parse' returns the snapshot shape: generated_at, one epic group, ungrouped."
  (let ((snap (epicworkmgr--parse (epicworkmgr-tests--fixture "agenda-sample.json"))))
    (should (equal (alist-get 'generated_at snap)
                   "2026-07-05T19:37:15.832137+00:00"))
    (should (= (length (alist-get 'epics snap)) 1))
    (should (= (length (alist-get 'ungrouped snap)) 1))
    ;; false -> nil, null -> nil, empty array -> nil
    (let ((issue (car (alist-get 'issues (car (alist-get 'epics snap))))))
      (should (eq (alist-get 'active issue) t))
      (should (null (alist-get 'due_date issue)))
      (should (null (alist-get 'blocked_by issue))))))

(ert-deftest epicworkmgr-test-host-from-snapshot ()
  "`--host-from-snapshot' derives scheme+host from the first issue's web_url."
  (let ((snap '((epics . (((issues . (((web_url . "https://gitlab.example.com/a/-/work_items/1"))))))))))
    (should (equal (epicworkmgr--host-from-snapshot snap)
                   "https://gitlab.example.com"))))

(ert-deftest epicworkmgr-test-host-falls-back-to-ungrouped ()
  "`--host-from-snapshot' uses ungrouped when there are no epic groups."
  (let ((snap '((epics . nil)
                (ungrouped . (((web_url . "https://gitlab.example.com/b/-/work_items/2")))))))
    (should (equal (epicworkmgr--host-from-snapshot snap)
                   "https://gitlab.example.com"))))

(ert-deftest epicworkmgr-test-blocker-url ()
  "`--blocker-url' rebuilds a work-items URL from a ref and host."
  (should (equal (epicworkmgr--blocker-url "grp/sub/proj#5" "https://h")
                 "https://h/grp/sub/proj/-/work_items/5")))

(ert-deftest epicworkmgr-test-format-generated-at ()
  "`--format-generated-at' renders HH:MM; ZONE t gives UTC, deterministic."
  (should (equal (epicworkmgr--format-generated-at
                  "2026-07-05T19:37:15.832137+00:00" t)
                 "19:37")))

;;; --- Cycle 2: icon helpers ---

(ert-deftest epicworkmgr-test-priority-icon ()
  "`--priority-icon' maps A/B/C to their codepoints with the priority face."
  (let ((icon (epicworkmgr--priority-icon "A")))
    (should (equal (substring-no-properties icon) (char-to-string #xf2c7)))
    (should (eq (get-text-property 0 'font-lock-face icon) 'epicworkmgr-priority-a)))
  (should (equal (substring-no-properties (epicworkmgr--priority-icon "B"))
                 (char-to-string #xf2c9)))
  (should (equal (substring-no-properties (epicworkmgr--priority-icon "C"))
                 (char-to-string #xf2cb))))

(ert-deftest epicworkmgr-test-blocked-marker ()
  "`--blocked-marker' shows the icon + count when blocked, \"\" when not."
  (let ((marker (epicworkmgr--blocked-marker 2)))
    (should (equal (substring-no-properties marker)
                   (concat (char-to-string #xf256) "2")))
    (should (eq (get-text-property 0 'font-lock-face marker) 'epicworkmgr-blocked)))
  (should (equal (epicworkmgr--blocked-marker 0) "")))

(ert-deftest epicworkmgr-test-active-marker ()
  "`--active-marker' is the active-icon codepoint in the active face."
  (let ((marker (epicworkmgr--active-marker)))
    (should (equal (substring-no-properties marker) (char-to-string #xeb79)))
    (should (eq (get-text-property 0 'font-lock-face marker) 'epicworkmgr-active))))

;;; --- Cycle 3: issue line ---

(ert-deftest epicworkmgr-test-issue-line-ready ()
  "An active, priority-A issue leads with the active + priority icons, active-faced."
  (let* ((issue '((priority . "A") (project . "widget-api") (iid . 1)
                  (title . "Add health-check endpoint")
                  (active . t) (blocked_by . nil)))
         (line (epicworkmgr--issue-line issue))
         (plain (substring-no-properties line)))
    (should (equal plain
                   (concat (char-to-string #xeb79) " " (char-to-string #xf2c7)
                           " widget-api#1  Add health-check endpoint")))
    (should (text-property-any 0 (length line) 'font-lock-face 'epicworkmgr-active line))
    (should (text-property-any 0 (length line) 'font-lock-face 'epicworkmgr-project line))))

(ert-deftest epicworkmgr-test-issue-line-blocked ()
  "A blocked, non-active issue carries the blocked marker and no active face."
  (let* ((issue '((priority . "B") (project . "data-loader") (iid . 1)
                  (title . "Backfill nightly export") (active . nil)
                  (blocked_by . (((ref . "x#1"))))))
         (line (epicworkmgr--issue-line issue))
         (plain (substring-no-properties line)))
    (should (equal plain
                   (concat (char-to-string #xf2c9)
                           " data-loader#1  Backfill nightly export  "
                           (char-to-string #xf256) "1")))
    (should-not (text-property-any 0 (length line) 'font-lock-face 'epicworkmgr-active line))))

;;; --- Cycle 4: project resolution ---

(ert-deftest epicworkmgr-test-project-root-basename-match ()
  "`--project-root' finds a known project by directory basename."
  (let ((projectile-known-projects
         '("/a/b/widget-api/" "/c/other-thing/")))
    (should (equal (epicworkmgr--project-root "widget-api")
                   "/a/b/widget-api/"))
    (should (null (epicworkmgr--project-root "not-checked-out")))))

;;; --- Cycle 5: render snapshot into buffer ---

(ert-deftest epicworkmgr-test-render-snapshot-text ()
  "Rendering the fixture emits the epic, both issues, Ungrouped, and a blocker."
  (let ((snap (epicworkmgr--parse (epicworkmgr-tests--fixture "agenda-sample.json"))))
    (with-temp-buffer
      (magit-section-mode)
      (epicworkmgr--render-snapshot snap)
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "42 Example Programme" text))
        (should (string-match-p "widget-api#1" text))
        (should (string-match-p "data-loader#1" text))
        (should (string-match-p "Ungrouped" text))
        (should (string-match-p "auth-service#1" text))
        (should (string-match-p "report-ui#1" text))))))

(ert-deftest epicworkmgr-test-render-snapshot-structure ()
  "Rendering builds root -> epic(2 issues) + ungrouped, blockers under the blocked issue."
  (let ((snap (epicworkmgr--parse (epicworkmgr-tests--fixture "agenda-sample.json"))))
    (with-temp-buffer
      (magit-section-mode)
      (epicworkmgr--render-snapshot snap)
      (let* ((root magit-root-section)
             (epic (car (oref root children)))
             (ungrouped (car (last (oref root children)))))
        (should (eq (oref epic type) 'epicworkmgr-epic))
        (should (= (length (oref epic children)) 2))
        (let ((ready (nth 0 (oref epic children)))
              (blocked (nth 1 (oref epic children))))
          (should (eq (oref ready type) 'epicworkmgr-issue))
          (should (null (oref ready children)))
          (should (= (length (oref blocked children)) 1))
          (should (eq (oref (car (oref blocked children)) type)
                      'epicworkmgr-blockers))
          ;; the "blocked by" section starts collapsed
          (should (oref (car (oref blocked children)) hidden)))
        (should (eq (oref ungrouped type) 'epicworkmgr-ungrouped))))))

;;; --- Cycle 6: header-line state (loading indicator) ---

(ert-deftest epicworkmgr-test-header-fetching ()
  "The fetching header names the fetch and carries no timestamp."
  (with-temp-buffer
    (epicworkmgr--set-header 'fetching)
    (should (string-match-p "Fetching" header-line-format))))

(ert-deftest epicworkmgr-test-header-refreshing ()
  "The refreshing header shows it is refreshing over the still-visible stamp."
  (with-temp-buffer
    (epicworkmgr--set-header 'refreshing "2026-07-05T19:37:15+00:00")
    (should (string-match-p "Refreshing" header-line-format))
    (should (string-match-p "showing" header-line-format))
    (should (string-match-p "[0-9][0-9]:[0-9][0-9]" header-line-format))))

(ert-deftest epicworkmgr-test-header-idle ()
  "The idle header shows the synced stamp and the refresh hint."
  (with-temp-buffer
    (epicworkmgr--set-header 'idle "2026-07-05T19:37:15+00:00")
    (should (string-match-p "synced" header-line-format))
    (should (string-match-p "[0-9][0-9]:[0-9][0-9]" header-line-format))
    (should (string-match-p "\\[g\\] refresh" header-line-format))))

;;; --- Cycle 7: interaction, mode & keymap ---

(ert-deftest epicworkmgr-test-section-target-url ()
  "`--section-target-url' returns a URL string as-is and an alist's web_url."
  (should (equal (epicworkmgr--section-target-url "https://h/x") "https://h/x"))
  (should (equal (epicworkmgr--section-target-url '((web_url . "https://h/y")))
                 "https://h/y"))
  (should (null (epicworkmgr--section-target-url nil))))

(ert-deftest epicworkmgr-test-visit-issue ()
  "`epicworkmgr-visit' browses the web_url of the issue section at point."
  (let ((snap (epicworkmgr--parse (epicworkmgr-tests--fixture "agenda-sample.json")))
        (visited nil))
    (with-temp-buffer
      (magit-section-mode)
      (epicworkmgr--render-snapshot snap)
      (goto-char (point-min))
      (search-forward "widget-api#1")
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _) (setq visited url))))
        (epicworkmgr-visit))
      (should (equal visited
                     "https://gitlab.example.com/acme/team/widget-api/-/work_items/1")))))

(ert-deftest epicworkmgr-test-jump-resolves ()
  "`epicworkmgr-jump-to-project' switches to the matched local checkout."
  (let ((snap (epicworkmgr--parse (epicworkmgr-tests--fixture "agenda-sample.json")))
        (projectile-known-projects '("/x/widget-api/"))
        (switched nil))
    (with-temp-buffer
      (magit-section-mode)
      (epicworkmgr--render-snapshot snap)
      (goto-char (point-min))
      (search-forward "widget-api#1")
      (cl-letf (((symbol-function 'epicworkmgr--switch-project-other-window)
                 (lambda (root) (setq switched root))))
        (epicworkmgr-jump-to-project))
      (should (equal switched "/x/widget-api/")))))

(ert-deftest epicworkmgr-test-jump-no-checkout ()
  "With no local checkout, jump reports it and does not switch."
  (let ((snap (epicworkmgr--parse (epicworkmgr-tests--fixture "agenda-sample.json")))
        (projectile-known-projects nil)
        (msg nil) (switched nil))
    (with-temp-buffer
      (magit-section-mode)
      (epicworkmgr--render-snapshot snap)
      (goto-char (point-min))
      (search-forward "widget-api#1")
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (setq msg (apply #'format fmt args))))
                ((symbol-function 'epicworkmgr--switch-project-other-window)
                 (lambda (root) (setq switched root))))
        (epicworkmgr-jump-to-project))
      (should (string-match-p "widget-api" msg))
      (should (string-match-p "no local checkout" msg))
      (should (null switched)))))

(ert-deftest epicworkmgr-test-mode-keymap ()
  "`epicworkmgr-mode' derives from magit-section-mode and binds the action keys."
  (should (eq (get 'epicworkmgr-mode 'derived-mode-parent) 'magit-section-mode))
  (should (eq (lookup-key epicworkmgr-mode-map (kbd "RET")) 'epicworkmgr-visit))
  (should (eq (lookup-key epicworkmgr-mode-map (kbd "g")) 'epicworkmgr-refresh))
  (should (eq (lookup-key epicworkmgr-mode-map (kbd "o")) 'epicworkmgr-jump-to-project)))

(provide 'init-epicworkmgr-tests)
;;; init-epicworkmgr-tests.el ends here
