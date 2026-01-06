;;; init-icons.el --- Icon configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup nerd-icons for use throughout Emacs
;;; Code:

(use-package nerd-icons
  :ensure t
  :demand t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  :config
  ;; Make Emacs use nerd font for symbol ranges (PUA where nerd icons live)
  (when (display-graphic-p)
    (set-fontset-font t 'symbol "Symbols Nerd Font Mono" nil 'prepend)
    (set-fontset-font t '(#xe000 . #xf8ff) "Symbols Nerd Font Mono" nil 'prepend)
    (set-fontset-font t '(#xf0000 . #xfffff) "Symbols Nerd Font Mono" nil 'prepend)))

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'init-icons)
;;; init-icons.el ends here
