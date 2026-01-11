;;; early-init.el --- Early initialization -*- lexical-binding: t -*-
;;; Commentary:
;; Runs before init.el. Used to disable package.el since nix manages packages.
;;; Code:

;; Disable package.el - all packages managed by nix
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
