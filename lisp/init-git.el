;;; init-git.el --- Git/Magit configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup magit and version control
;;; Code:

(use-package magit
  :ensure t
  :bind (("C-c m" . magit-status))
  :config
  (defun my-magit-side-by-side (orig-fun &rest args)
    "Run magit-status with side-by-side split."
    (let ((split-height-threshold nil)
          (split-width-threshold 0))
      (apply orig-fun args)))
  (advice-add 'magit-status :around #'my-magit-side-by-side))

(use-package vc
  :config
  (defun my-vc-diff-side-by-side (orig-fun &rest args)
    "Run vc-diff with side-by-side split."
    (let ((split-height-threshold nil)
          (split-width-threshold 0))
      (apply orig-fun args)))
  (advice-add 'vc-diff :around #'my-vc-diff-side-by-side))

(use-package ediff
  :config
  (setq ediff-shell (getenv "$SHELL"))
  (setq-default ediff-split-window-function
                (quote split-window-vertically)))

(use-package gist
  :ensure t)

(provide 'init-git)
;;; init-git.el ends here
