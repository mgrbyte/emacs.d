;;; init-osx.el --- macOS configuration -*- lexical-binding: t -*-
;;; Commentary:
;; macOS-specific settings
;;; Code:

;; macOS: swap Command/Option - Command is Meta, Option passes through
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

(provide 'init-osx)
;;; init-osx.el ends here
