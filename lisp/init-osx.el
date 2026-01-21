;;; init-osx.el --- macOS configuration -*- lexical-binding: t -*-
;;; Commentary:
;; macOS-specific settings
;;; Code:

;; macOS: Command is Meta, Option passes through, right-Command is Super
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil)
  (setq ns-right-command-modifier 'super))

(provide 'init-osx)
;;; init-osx.el ends here
