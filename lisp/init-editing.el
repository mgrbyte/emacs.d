;;; init-editing.el --- Editing utilities -*- lexical-binding: t -*-
;;; Commentary:
;; Setup paredit, whitespace, and editing utilities
;;; Code:

(use-package paredit

  :diminish paredit-mode
  :init
  (enable-paredit-mode)
  :config
  (unbind-key "M-s" paredit-mode-map)
  (unbind-key "M-r" paredit-mode-map))

(use-package paren
  :config
  (setq show-paren-style 'parenthesis)
  :init
  (show-paren-mode 1))

(use-package rainbow-delimiters

  :config
  (rainbow-delimiters-mode-enable))

(use-package whitespace-cleanup-mode

  :bind (("C-c _ w" . whitespace-mode)
         ("C-c _ t" . whitespace-toggle-options)
         ("C-c = w" . global-whitespace-mode)
         ("C-c = t" . global-whitespace-toggle-options))
  :config
  (setq-default whitespace-cleanup-mode-preserve-point t)
  (setq-default whitespace-cleanup-mode-only-if-initially-clean t)
  (setq-default whitespace-cleanup-mode-ignore-modes
                '(helm-mode
                  special-mode
                  view-mode)))

(use-package editorconfig

  :config
  (editorconfig-mode 1)
  ;; Skip editorconfig for remote files (walks directory tree over TRAMP)
  (defun mgrbyte-editorconfig-skip-remote (orig-fun &rest args)
    "Return empty properties for remote files instead of walking the tree."
    (if (file-remote-p (or buffer-file-name default-directory ""))
        (make-hash-table :test 'equal)
      (apply orig-fun args)))
  (advice-add 'editorconfig-core-get-properties-hash :around #'mgrbyte-editorconfig-skip-remote))

(use-package keyfreq

  :config
  (keyfreq-mode))

(use-package zygospore)

;; Rebind query-replace since M-% conflicts with macOS screenshot
(global-set-key (kbd "C-c r") 'query-replace)
(global-set-key (kbd "C-c R") 'query-replace-regexp)

(provide 'init-editing)
;;; init-editing.el ends here
