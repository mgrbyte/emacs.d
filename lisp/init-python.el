;;; init-python.el --- Python configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup Python mode, LSP (ty + ruff), pytest
;;; Code:

(use-package python
  :bind (("C-c d i" . py-insert-debug)
         ("RET" . newline-and-indent))
  :mode (("\\.py$" . python-mode)
         ("\\.cpy$" . python-mode)
         ("\\.vpy$" . python-mode))
  :config
  (declare-function py-insert-debug mgrbyte nil)
  (setq fill-column 79)
  (setq tab-width 4))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-chars 1)
  (setq company-idle-delay 0.05)
  (setq company-format-margin-function 'company-text-icons-margin)
  (set-face-attribute 'company-tooltip nil
                      :background "black" :foreground "white")
  (set-face-attribute 'company-tooltip-selection nil
                      :background "transparent" :foreground "yellow")
  (set-face-attribute 'company-tooltip-common nil
                      :foreground "yellow")
  (set-face-attribute 'company-tooltip-common-selection nil
                      :foreground "cyan"))

(use-package python-pytest
  :bind (:map python-mode-map
         ("C-c p p" . python-pytest-dispatch)
         ("C-c p f" . python-pytest-file)
         ("C-c p ." . python-pytest-run-def-or-class-at-point)
         ("C-c p r" . python-pytest-repeat)
         ("C-c p l" . python-pytest-last-failed))
  :config
  (setq python-pytest-executable "uv run pytest"))

(use-package yasnippet
  :hook (python-mode . yas-minor-mode))

;; Register ty (Astral's type checker) as Python LSP server
;; ty provides: type checking, completions, go-to-definition, hover
(use-package lsp-mode
  :config
  (require 'lsp-ruff)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ty" "server"))
    :activation-fn (lsp-activate-on "python")
    :server-id 'ty
    :priority 1
    :add-on? nil))
  (add-to-list 'lsp-disabled-clients 'pylsp)
  (add-to-list 'lsp-disabled-clients 'pyright))

;; Python mode LSP hook - start ty + ruff
(add-hook 'python-mode-hook
          (lambda ()
            (lsp)
            (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

(use-package py-snippets
  :after yasnippet
  :config
  (py-snippets-initialize))

(provide 'init-python)
;;; init-python.el ends here
