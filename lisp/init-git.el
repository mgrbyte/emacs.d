;;; init-git.el --- Git/Magit configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup magit and version control
;;; Code:

(use-package magit

  :bind (("C-c m" . magit-status))
  :config
  (defun mgrbyte-magit-side-by-side (orig-fun &rest args)
    "Run magit-status with side-by-side split."
    (let ((split-height-threshold nil)
          (split-width-threshold 0))
      (apply orig-fun args)))
  (advice-add 'magit-status :around #'mgrbyte-magit-side-by-side)
  ;; TRAMP performance: reduce magit round-trips on remote repos
  (setq magit-tramp-pipe-stty-settings 'pty)
  (setq magit-commit-show-diff nil)
  (setq magit-branch-direct-configure nil)
  (setq magit-refresh-status-buffer nil))

(use-package vc
  :config
  (defun mgrbyte-vc-diff-side-by-side (orig-fun &rest args)
    "Run vc-diff with side-by-side split."
    (let ((split-height-threshold nil)
          (split-width-threshold 0))
      (apply orig-fun args)))
  (advice-add 'vc-diff :around #'mgrbyte-vc-diff-side-by-side))

(use-package ediff
  :config
  (setq ediff-shell (getenv "$SHELL"))
  ;; In daemon mode cannot create new top-level frames programmatically
  ;; when using GUI on MacOS (Quartz) nor NixOS (GTK3), causing a crash,
  ;; thus fallback to using plain (single-frame) layout for ediff's control panel.
  (with-eval-after-load 'ediff-wind
    (when (daemonp)
      (setq ediff-window-setup-function 'ediff-setup-windows-plain)))
  (setq-default ediff-split-window-function
                (quote split-window-vertically)))

(use-package gist)

(provide 'init-git)
;;; init-git.el ends here
