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
    :activation-fn (lambda (file-name mode)
                     (and (not (file-remote-p file-name))
                          (funcall (lsp-activate-on "python") file-name mode)))
    :server-id 'ty
    :priority 1
    :add-on? nil))
  (add-to-list 'lsp-disabled-clients 'pylsp)
  (add-to-list 'lsp-disabled-clients 'pyright)
  (add-to-list 'lsp-disabled-clients 'ty-ls)
  (add-to-list 'lsp-disabled-clients 'ty-ls-tramp)
  (add-to-list 'lsp-disabled-clients 'semgrep-ls-tramp)
  (add-to-list 'lsp-disabled-clients 'pyls-tramp)
  (add-to-list 'lsp-disabled-clients 'pylsp-tramp)
  ;; Re-register ruff to exclude remote files (lsp-ruff doesn't set :remote?)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("ruff" "server"))
    :activation-fn (lambda (file-name mode)
                     (and (not (file-remote-p file-name))
                          (funcall (lsp-activate-on "python") file-name mode)))
    :server-id 'ruff
    :add-on? t))
  ;; Register ty for TRAMP (works once ty is installed on remote host)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tramp-connection '("zsh" "-l" "-c"
                                            "exec nix-user-chroot ~/.nix ~/.local/bin/ty server")
                                          (lambda () t))
    :major-modes '(python-mode)
    :remote? t
    :server-id 'ty-tramp
    :priority 1
    :add-on? nil
    :initialization-options
    (lambda ()
      (list :logFile "/tmp/ty-server.log"
            :logLevel "debug"))))
  ;; Fix ~ not being expanded in rootUri for TRAMP paths.
  ;; lsp--path-to-uri-1 uses expand-file-name which doesn't resolve ~ for
  ;; TRAMP, so the server receives file://~/... and can't find project config.
  (defun mgrbyte-lsp-expand-remote-tilde (orig-fn path)
    "Resolve ~ in remote TRAMP paths before converting to URI."
    (if (and (file-remote-p path)
             (string-match-p "~" (or (file-remote-p path 'localname) "")))
        (funcall orig-fn (file-truename path))
      (funcall orig-fn path)))
  (advice-add 'lsp--path-to-uri-1 :around #'mgrbyte-lsp-expand-remote-tilde))

;; Increase lsp-response-timeout for remote buffers — ty needs longer to
;; respond over TRAMP, especially for the first completion (project indexing).
(defun mgrbyte-increase-lsp-timeout-for-remote ()
  "Use a longer LSP response timeout for remote files."
  (when (file-remote-p (or buffer-file-name default-directory ""))
    (setq-local lsp-response-timeout 30)))

;; Disable python indent guessing over TRAMP (hangs trying to run python remotely)
(defun mgrbyte-disable-indent-guess-for-tramp ()
  "Disable Python indent guessing for remote files."
  (when (file-remote-p (or buffer-file-name default-directory ""))
    (setq-local python-indent-guess-indent-offset nil)))
(add-hook 'python-mode-hook #'mgrbyte-disable-indent-guess-for-tramp)
(add-hook 'python-mode-hook #'mgrbyte-increase-lsp-timeout-for-remote)

;; Python mode LSP hook - start ty + ruff
;; Local clients exclude remote files; TRAMP clients handle remote.
;; Format with ruff on save (ty does not support documentFormattingProvider)
(add-hook 'python-mode-hook #'lsp)

(add-hook 'python-mode-hook
          (lambda ()
            (add-hook 'after-save-hook
                      (lambda ()
                        (when (and buffer-file-name
                                   (not (file-remote-p buffer-file-name)))
                          (let ((exit-code
                                 (call-process "uvx" nil nil nil
                                               "ruff" "format" buffer-file-name)))
                            (when (zerop exit-code)
                              (revert-buffer t t t)))))
                      nil t)))

(use-package py-snippets
  :after yasnippet
  :config
  (py-snippets-initialize))

(provide 'init-python)
;;; init-python.el ends here
