;;; init-icons.el --- Icon configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup nerd-icons for use throughout Emacs
;;; Code:

(use-package nerd-icons

  :demand t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  :config
  ;; Make Emacs use nerd font for symbol ranges (PUA where nerd icons live)
  (defun mgrbyte-setup-nerd-fonts ()
    "Configure fontset to use nerd fonts for icon codepoints."
    (set-fontset-font t 'symbol "Symbols Nerd Font Mono" nil 'prepend)
    (set-fontset-font t '(#xe000 . #xf8ff) "Symbols Nerd Font Mono" nil 'prepend)
    (set-fontset-font t '(#xf0000 . #xfffff) "Symbols Nerd Font Mono" nil 'prepend))
  ;; Run when GUI is available (handles daemon mode)
  (if (display-graphic-p)
      (mgrbyte-setup-nerd-fonts)
    (add-hook 'server-after-make-frame-hook #'mgrbyte-setup-nerd-fonts)))

(use-package nerd-icons-dired

  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'init-icons)
;;; init-icons.el ends here
