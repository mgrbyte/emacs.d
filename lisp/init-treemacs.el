;;; init-treemacs.el --- Treemacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup treemacs file explorer
;;; Code:

(use-package treemacs

  :bind (("C-c e" . treemacs)
         ("C-c E" . treemacs-select-window))
  :config
  (setq treemacs-width 35)
  (setq treemacs-position 'right)
  (setq treemacs-project-follow-cleanup t)
  ;; Hide gitignored files
  (treemacs-hide-gitignored-files-mode t)
  ;; Hide common dot files not in .gitignore
  (setq treemacs-file-ignore-extensions nil)
  (setq treemacs-file-ignore-globs
        '(".coverage"
          ".coveragerc"
          ".devcontainer"
          ".ropeproject"
          ".pre-commit-config.yaml"
          ".pre-commit-config.yml"
          ".cruft.json"
          "__pycache__"
          "*.pyc"
          ".mypy_cache"
          ".ruff_cache"
          ".pytest_cache"
          "*.egg-info"))
  (treemacs-project-follow-mode t))

(use-package treemacs-projectile

  :after (treemacs projectile))

(use-package treemacs-nerd-icons

  :demand t)

(use-package treemacs-magit

  :after (treemacs-nerd-icons treemacs-projectile))

(provide 'init-treemacs)
;;; init-treemacs.el ends here
