;;; init-dashboard.el --- Dashboard configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup emacs-dashboard as startup screen
;;; Code:

(use-package dashboard
  :ensure t
  :after (nerd-icons projectile)
  :config
  (setq dashboard-projects-backend 'projectile)

  ;; Centering and layout
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-startup-banner "~/Pictures/Logos/techiaith-swirl.png")
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

  ;; Agenda settings - show today and this week
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

  ;; Remove random footer messages
  (setq dashboard-footer-messages '(""))
  (setq dashboard-agenda-time-string-format "%d/%m/%Y")
  ;; Keep init info (packages loaded in N seconds)
  (setq dashboard-set-init-info t)
  (dashboard-setup-startup-hook))

(provide 'init-dashboard)
;;; init-dashboard.el ends here
