;;; init-tramp.el --- TRAMP remote editing configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Optimise TRAMP for remote development over SSH.
;; Reference: https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
;;; Code:

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-default-remote-shell "/bin/zsh")
  (setq tramp-encoding-shell "/bin/zsh")
  (setq tramp-shell-prompt-pattern
        "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")
  (setq tramp-verbose 1)
  (setq tramp-direct-async-process t)
  (setq tramp-use-ssh-controlmaster-options nil)
  (setq tramp-use-scp-direct-remote-copying t)
  (setq tramp-copy-size-limit (* 1024 1024))
  (setq tramp-histfile-override t)
  (setq tramp-auto-save-directory
        (expand-file-name "tramp-autosave" user-emacs-directory))
  (setq remote-file-name-inhibit-locks t)
  (setq remote-file-name-inhibit-auto-save-visited t)
  (setq remote-file-name-inhibit-cache nil)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; Disable eldoc and completion-at-point for remote buffers
  (defun mgrbyte-tramp-disable-expensive-modes ()
    "Disable eldoc and capf for remote buffers."
    (when (file-remote-p (or buffer-file-name default-directory ""))
      (eldoc-mode -1)
      (setq-local completion-at-point-functions nil)))
  (add-hook 'find-file-hook #'mgrbyte-tramp-disable-expensive-modes)

  ;; Skip treemacs project-follow over TRAMP (timer causes SSH round-trips)
  (defun mgrbyte-treemacs-follow-project-no-tramp (orig-fun &rest args)
    "Skip treemacs project following for remote buffers."
    (unless (file-remote-p default-directory)
      (apply orig-fun args)))
  (with-eval-after-load 'treemacs
    (advice-add 'treemacs--do-follow-project :around #'mgrbyte-treemacs-follow-project-no-tramp))

  ;; Skip project-try-vc over TRAMP (walks directory tree, each dir is a round-trip)
  (defun mgrbyte-project-try-vc-no-tramp (orig-fun dir)
    "Skip project-try-vc for TRAMP paths."
    (unless (file-remote-p dir)
      (funcall orig-fun dir)))
  (advice-add 'project-try-vc :around #'mgrbyte-project-try-vc-no-tramp)

  ;; Skip vc-refresh-state over TRAMP (runs git on every file open)
  (defun mgrbyte-vc-refresh-state-no-tramp (orig-fun &rest args)
    "Disable VC state refresh for TRAMP files."
    (unless (file-remote-p default-directory)
      (apply orig-fun args)))
  (advice-add 'vc-refresh-state :around #'mgrbyte-vc-refresh-state-no-tramp))

(provide 'init-tramp)
;;; init-tramp.el ends here
