;;; init-python.el --- Python configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup Python mode, LSP, pytest
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
  ;; High contrast faces for colour blindness
  ;; (setq company-tooltip-margin 3)
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

;; Load lsp-ruff early so it's registered before any Python buffer opens
;; Ruff runs as add-on server (`:add-on? t`) for formatting alongside pyright
(use-package lsp-mode
  :config
  (require 'lsp-ruff))

;; Use pyright for Python LSP (type checking and diagnostics)
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)
                          (add-hook 'before-save-hook #'lsp-organize-imports nil t)
                          (add-hook 'before-save-hook #'lsp-format-buffer nil t)))
  :config
  ;; Disable pylsp (replaced by pyright)
  (add-to-list 'lsp-disabled-clients 'pylsp))

(use-package py-snippets
  :after yasnippet
  :config
  (py-snippets-initialize))

(provide 'init-python)
;;; init-python.el ends here
