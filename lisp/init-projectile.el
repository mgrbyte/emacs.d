;;; init-projectile.el --- Projectile configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup projectile for project management
;;; Code:

(use-package projectile
  :ensure t
  :demand t
  :bind-keymap (("C-c p" . projectile-command-map))
  :bind (("C-c k" . mgrbyte-show-dashboard))
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'helm)
  (setq projectile-sort-order 'recentf)
  (setq projectile-switch-project-action (lambda () (mgrbyte-project-layout)))
  (setq projectile-project-search-path '(("~/github/mgrbyte" . 1)
                                         ("~/gitlab/mtr21pqh" . 2)
                                         ("~/gitlab/cyfieithu-ac-llms" . 3))))

(provide 'init-projectile)
;;; init-projectile.el ends here
