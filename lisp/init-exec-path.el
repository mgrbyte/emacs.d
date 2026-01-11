;;; init-exec-path.el --- Shell environment -*- lexical-binding: t -*-
;;; Commentary:
;; Setup exec-path-from-shell to inherit shell environment
;;; Code:

(use-package exec-path-from-shell

  :config
  (setq exec-path-from-shell-arguments '("-l" "-i"))
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
