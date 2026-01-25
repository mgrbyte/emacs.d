;;; init-lsp.el --- LSP configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Shared LSP configuration for all language modes.
;;; Code:

(use-package lsp-mode
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-snippet nil)
  :bind (:map lsp-mode-map
         ("C-c f" . lsp-format-buffer)
         ("C-c o" . lsp-organize-imports)
         ("C-c l r" . lsp-find-references)
         ("C-c l R" . lsp-rename)
         ("C-c l d" . lsp-find-definition))
  :config
  (setq lsp-auto-guess-root t)
  ;; Headerline breadcrumbs - more visible colours (abyss theme friendly)
  (setq lsp-headerline-breadcrumb-enable t)
  (with-eval-after-load 'lsp-headerline
    (set-face-attribute 'lsp-headerline-breadcrumb-path-face nil
                        :foreground "#7DCFFF" :weight 'normal)
    (set-face-attribute 'lsp-headerline-breadcrumb-symbols-face nil
                        :foreground "#E0AF68" :weight 'bold)
    (set-face-attribute 'lsp-headerline-breadcrumb-separator-face nil
                        :foreground "#787C99")
    ;; Match lsp-ui-doc background (#050000)
    (set-face-attribute 'header-line nil
                        :background "#050000")))

(use-package lsp-ui
  :after lsp-mode
  :bind (:map lsp-ui-mode-map
         ("C-c l p d" . lsp-ui-peek-find-definitions)
         ("C-c l p r" . lsp-ui-peek-find-references))
  :config
  ;; Doc - childframe at top-right
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-side 'right)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-use-webkit nil)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-max-height 50)
  (setq lsp-ui-doc-text-scale-level 1)
  ;; Header face - bigger title
  (set-face-attribute 'lsp-ui-doc-header nil :height 1.5 :weight 'bold)
  ;; Sideline - diagnostics and code actions only (no hover docs)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-code-actions t)
  ;; Peek - inline xref
  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-show-directory t)
  ;; remap xref-find-{definitions,references} (bound to M-. M-? by default
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))


(provide 'init-lsp)
;;; init-lsp.el ends here
