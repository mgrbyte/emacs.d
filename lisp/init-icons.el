;;; init-icons.el --- Icon configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup nerd-icons for use throughout Emacs
;;; Code:

(use-package nerd-icons
  :ensure t
  :demand t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'init-icons)
;;; init-icons.el ends here
