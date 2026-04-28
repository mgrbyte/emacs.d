;;; init-projectile.el --- Projectile configuration -*- lexical-binding: t -*1-
;;; Commentary:
;; Setup projectile for project management
;;; Code:

(use-package projectile

  :demand t
  :bind-keymap (("C-c p" . projectile-command-map))
  :bind (("<home>" . mgrbyte-show-dashboard))
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'helm)
  (setq projectile-sort-order 'recentf)
  (setq projectile-switch-project-action (lambda () (mgrbyte-project-layout)))
  (setq projectile-project-search-path '(("~/github/mgrbyte" . 1)
                                         ("~/gitlab/mtr21pqh" . 2)
                                         ("~/gitlab/cyfieithu-ac-llms" . 3)
                                         ("~/huggingface/spaces" . 2)))

  ;; Recognise known projects immediately without filesystem access.
  ;; Prevents TRAMP round-trips when opening remote projects from the dashboard.
  (defun mgrbyte-projectile-root-known (dir &optional _list)
    "Return known project root matching DIR without filesystem access."
    (cl-find-if (lambda (known) (string-prefix-p known dir))
                projectile-known-projects))
  (add-to-list 'projectile-project-root-functions #'mgrbyte-projectile-root-known))

(provide 'init-projectile)
;;; init-projectile.el ends here
