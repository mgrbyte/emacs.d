;;; init-editing.el --- Editing utilities -*- lexical-binding: t -*-
;;; Commentary:
;; Setup paredit, whitespace, and editing utilities
;;; Code:

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (enable-paredit-mode)
  :config
  (unbind-key "M-s" paredit-mode-map)
  (unbind-key "M-r" paredit-mode-map))

(use-package paren
  :config
  (setq show-paren-style 'expression)
  :init
  (show-paren-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :config
  (rainbow-delimiters-mode-enable))

(use-package whitespace-cleanup-mode
  :ensure t
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
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode))

(use-package zygospore :ensure t)

(provide 'init-editing)
;;; init-editing.el ends here
