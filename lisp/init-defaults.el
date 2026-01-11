;;; init-defaults.el --- General settings -*- lexical-binding: t -*-
;;; Commentary:
;; Default settings, backups, version control, misc
;;; Code:

;; Backups
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      delete-old-versions t
      kept-new-versions 2
      kept-old-versions 5)

;; Version Control
(setq-default version-control t)
(setq-default vc-follow-symlinks t)
(setq vc-handled-backends '(Git))

;; Indentation
(setq-default indent-line-function 'insert-tab)
(setq indent-tabs-mode nil)
(setq tab-always-indent nil)

;; Scrolling - do not add newlines when cursoring past last line in file
(setq scroll-step 1)
(setq next-line-add-newlines nil)

;; Display
(setq transient-mark-mode t)
(setq column-number-mode t)
(setq inhibit-startup-message t)
(setq search-highlight t)
(setq query-replace-highlight t)

;; Desktop mode - remember working files
(setq desktop-save-mode t)

;; Misc settings
(setq mail-interactive t)

;; Annoyance factor
(fset 'yes-or-no-p 'y-or-n-p)
(setq font-lock-verbose nil)
(setq confirm-nonexistent-file-or-buffer nil)

;; Un-disable some 'dangerous!' commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Avoid audio beeping
(setq visible-bell nil)
(setq debug-on-error t)

;; Themes
(setq custom-theme-directory (locate-user-emacs-file "themes"))
(setq-default theme-load-from-file t)

;; File associations
(add-to-list 'auto-mode-alist '("Makfile.*" . makefile-gmake-mode))

;; Auth sources
(use-package auth-source
  :after dash
  :config
  (setq auth-source-debug t)
  (setq auth-sources (-filter #'file-exists-p '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))))

;; Abyss theme
(use-package abyss-theme)

(provide 'init-defaults)
;;; init-defaults.el ends here
