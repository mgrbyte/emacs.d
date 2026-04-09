;;; init-treemacs.el --- Treemacs configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup treemacs file explorer
;;; Code:

(defun mgrbyte-treemacs-fit-width ()
  "Resize the treemacs window to fit the longest visible line plus a small margin."
  (when-let* ((buf (treemacs-get-local-buffer))
              (win (get-buffer-window buf t)))
    (with-current-buffer buf
      (let ((max-len 0))
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (end-of-line)
            (setq max-len (max max-len (current-column)))
            (forward-line 1)))
        (let ((new-width (+ max-len 2)))
          (unless (= new-width (window-width win))
            (setq treemacs-width new-width)
            (window-resize win (- new-width (window-width win)) t)))))))

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
  (treemacs-project-follow-mode t)
  (add-hook 'treemacs-post-project-refresh-functions #'mgrbyte-treemacs-fit-width)
  (add-hook 'treemacs-after-node-open-close-functions
            (lambda (_) (mgrbyte-treemacs-fit-width))))

(use-package treemacs-projectile

  :after (treemacs projectile))

(use-package treemacs-nerd-icons

  :demand t)

(use-package treemacs-magit

  :after (treemacs-nerd-icons treemacs-projectile))

(provide 'init-treemacs)
;;; init-treemacs.el ends here
