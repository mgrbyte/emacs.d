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
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action (lambda () (mgrbyte-project-layout)))
  (setq projectile-project-search-path '(("~/github/mgrbyte" . 1)
                                         ("~/gitlab/mtr21pqh" . 2)
                                         ("~/gitlab/cyfieithu-ac-llms" . 3)
                                         ("~/huggingface/spaces" . 2)))

  ;; For remote paths, projectile-project-root bails out before calling
  ;; root functions if the TRAMP connection isn't active. Advise it to
  ;; check known projects first, avoiding the connection requirement.
  (defun mgrbyte-projectile-root-known (dir)
    "Return known project root matching DIR without filesystem access."
    (cl-find-if (lambda (known) (string-prefix-p known dir))
                projectile-known-projects))

  (defun mgrbyte-projectile-root-no-tramp-check (orig-fun &optional dir)
    "Check known projects before TRAMP connection check in projectile-project-root."
    (let* ((d (or dir default-directory))
           (known (and (file-remote-p d) (mgrbyte-projectile-root-known d))))
      (or known (funcall orig-fun dir))))
  (advice-add 'projectile-project-root :around #'mgrbyte-projectile-root-no-tramp-check))

(provide 'init-projectile)
;;; init-projectile.el ends here
