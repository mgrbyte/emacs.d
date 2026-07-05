;;; init-epicworkmgr-tests.el --- Tests for init-epicworkmgr -*- lexical-binding: t -*-
;;; Commentary:
;; ERT tests for the epicworkmgr agenda renderer.  Run in the live daemon via
;; the Emacs MCP: load-file the impl then this file, then select and run the
;; "epicworkmgr-" tests.  All fixture/test data is synthetic (no real hosts,
;; projects, or paths) so this file is safe in a public repository.
;;; Code:

(require 'ert)
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
    (should (eq (get-text-property 0 'face icon) 'epicworkmgr-priority-a)))
  (should (equal (substring-no-properties (epicworkmgr--priority-icon "B"))
                 (char-to-string #xf2c9)))
  (should (equal (substring-no-properties (epicworkmgr--priority-icon "C"))
                 (char-to-string #xf2cb))))

(ert-deftest epicworkmgr-test-blocked-marker ()
  "`--blocked-marker' shows the icon + count when blocked, \"\" when not."
  (let ((marker (epicworkmgr--blocked-marker 2)))
    (should (equal (substring-no-properties marker)
                   (concat (char-to-string #xf256) "2")))
    (should (eq (get-text-property 0 'face marker) 'epicworkmgr-blocked)))
  (should (equal (epicworkmgr--blocked-marker 0) "")))

;;; --- Cycle 3: issue line ---

(ert-deftest epicworkmgr-test-issue-line-ready ()
  "A ready, active, priority-A issue: icon + locator + title, active-faced, no marker."
  (let* ((issue '((priority . "A") (project . "widget-api") (iid . 1)
                  (title . "Add health-check endpoint")
                  (active . t) (blocked_by . nil)))
         (line (epicworkmgr--issue-line issue))
         (plain (substring-no-properties line)))
    (should (equal plain
                   (concat (char-to-string #xf2c7)
                           " widget-api#1  Add health-check endpoint")))
    (should (text-property-any 0 (length line) 'face 'epicworkmgr-active line))))

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
    (should-not (text-property-any 0 (length line) 'face 'epicworkmgr-active line))))

;;; --- Cycle 4: project resolution ---

(ert-deftest epicworkmgr-test-project-root-basename-match ()
  "`--project-root' finds a known project by directory basename."
  (let ((projectile-known-projects
         '("/a/b/widget-api/" "/c/other-thing/")))
    (should (equal (epicworkmgr--project-root "widget-api")
                   "/a/b/widget-api/"))
    (should (null (epicworkmgr--project-root "not-checked-out")))))

(provide 'init-epicworkmgr-tests)
;;; init-epicworkmgr-tests.el ends here
