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

(use-package lsp-mode

  :commands lsp
  :hook ((python-mode . lsp)
         (python-mode . (lambda ()
                          (add-hook 'before-save-hook #'lsp-organize-imports nil t)
                          (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
  :bind (:map lsp-mode-map
         ("C-c f" . lsp-format-buffer)
         ("C-c o" . lsp-organize-imports)
         ("C-c l r" . lsp-find-references)
         ("C-c l R" . lsp-rename)
         ("C-c l d" . lsp-find-definition))
  :config
  (setq lsp-auto-guess-root t)
  ;; Disable ruff LSP server (we use python-lsp-ruff plugin in pylsp instead)
  (add-to-list 'lsp-disabled-clients 'ruff)
  (setq lsp-pylsp-plugins-ruff-enabled t)
  (setq lsp-pylsp-plugins-ruff-format-enabled t)
  (setq lsp-pylsp-plugins-mypy-enabled t)
  (setq lsp-pylsp-plugins-mypy-live-mode nil)  ; Run mypy on save only for speed
  (setq lsp-pylsp-plugins-mypy-dmypy nil)
  ;; Disable built-in linters (ruff handles these)
  (setq lsp-pylsp-plugins-pycodestyle-enabled nil)
  (setq lsp-pylsp-plugins-pyflakes-enabled nil)
  (setq lsp-pylsp-plugins-mccabe-enabled nil)
  (setq lsp-completion-show-kind nil))

(use-package lsp-ui

  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
         ("C-?" . lsp-ui-doc-glance))
  :config
  (setq lsp-ui-sideline-enable t)
  ;; Disable auto-show to prevent accidental insertion
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil))

(use-package py-snippets

  :after yasnippet
  :config
  (py-snippets-initialize))

(provide 'init-python)
;;; init-python.el ends here
