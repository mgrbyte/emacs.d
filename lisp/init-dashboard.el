;;; init-dashboard.el --- Dashboard configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup emacs-dashboard as startup screen
;;; Code:

(use-package dashboard

  :after (nerd-icons projectile mgrbyte)
  :config
  (setq dashboard-projects-backend 'projectile)

  ;; Centering and layout
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-startup-banner (expand-file-name "images/techiaith-swirl.png" user-emacs-directory))
  (setq dashboard-banner-logo-title "Techiaith")

  ;; Project(ile)
  (setq dashboard-projects-switch-function
        (lambda (project-dir)
          (projectile-switch-project-by-name project-dir)))

  ;; Items to show
  (setq dashboard-items '((agenda . 20)
                          (projects . 15)
                          (recents  . 10)
                          (bookmarks . 10)))

  ;; Agenda settings - show all scheduled items
  (setq dashboard-agenda-release-buffers nil)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  (setq dashboard-week-agenda t)

  ;; Icons
  (setq dashboard-heading-icon-height 1.5)
  (setq dashboard-heading-icon-v-adjust -0.125)
  (setq dashboard-icon-file-height 1.5)
  (setq dashboard-icon-file-v-adjust -0.125)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-remote-path-icon
        (nerd-icons-mdicon "nf-md-ssh"
                           :height dashboard-icon-file-height
                           :v-adjust dashboard-icon-file-v-adjust))

  ;; Footer messages from CLAUDE_TIPS_FILE
  (setq dashboard-footer-messages (mgrbyte-dashboard-motd-messages))
  (setq dashboard-agenda-time-string-format "%d/%m/%Y")
  ;; Keep init info (packages loaded in N seconds)
  (setq dashboard-set-init-info t)

  ;; Show dashboard in new frames (works with emacsclient)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook)

  ;; In daemon mode, the dashboard renders before nerd fonts are available.
  ;; Refresh once after the first graphical frame is created.
  (when (daemonp)
    (defun mgrbyte-dashboard-refresh-after-frame ()
      "Refresh dashboard to pick up nerd-icons after frame creation."
      (when (get-buffer "*dashboard*")
        (dashboard-refresh-buffer))
      (remove-hook 'server-after-make-frame-hook #'mgrbyte-dashboard-refresh-after-frame))
    (add-hook 'server-after-make-frame-hook #'mgrbyte-dashboard-refresh-after-frame)))

;; Prune stale recentf entries, rescan org-agenda-files, and re-sync
;; GitLab before each dashboard refresh so new/removed org files,
;; deleted recent files, and GitLab changes are reflected immediately.
(defvar mgrbyte-dashboard--refreshing nil
  "Non-nil while dashboard is being refreshed, prevents re-entrancy.")

(define-advice dashboard-refresh-buffer (:before (&rest _) mgrbyte-refresh-sources)
  "Rescan org-agenda-files, prune recentf, and sync GitLab before refresh."
  (when (fboundp 'mgrbyte-org-agenda-files)
    (setq org-agenda-files (mgrbyte-org-agenda-files)))
  (when (bound-and-true-p recentf-mode)
    (recentf-cleanup))
  (when (and (not mgrbyte-dashboard--refreshing)
             (fboundp 'mgrbyte-gitlab-sync))
    (run-with-idle-timer
     1 nil
     (lambda ()
       (mgrbyte-gitlab-sync)
       (when (get-buffer "*dashboard*")
         (let ((mgrbyte-dashboard--refreshing t))
           (dashboard-refresh-buffer)))))))

;; Prevent dashboard from interfering with magit
(with-eval-after-load 'magit
  (add-hook 'magit-status-mode-hook
            (lambda ()
              (when-let ((buf (get-buffer "*dashboard*")))
                (kill-buffer buf)))))

;; Never display dashboard over other buffers
(add-to-list 'display-buffer-alist
             '("\\*dashboard\\*"
               (display-buffer-same-window)
               (inhibit-switch-frame . t)))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
