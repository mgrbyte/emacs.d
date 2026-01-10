;;; init-spell.el --- Spell checking with hunspell -*- lexical-binding: t -*-
;;; Commentary:
;; Configure hunspell for spell checking with Welsh and English dictionaries
;;; Code:

(use-package ispell
  :bind (("C-c s b" . ispell-buffer)
         ("C-c s w" . ispell-word)
         ("C-c s r" . ispell-region)
         ("C-c s d" . ispell-change-dictionary))
  :config
  ;; Set DICPATH for hunspell (emacs daemon doesn't inherit shell env)
  (setenv "DICPATH"
          (concat (expand-file-name "~/.local/share/hunspell") ":"
                  (expand-file-name "~/.nix-profile/share/hunspell")))
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_GB")
  (setq ispell-local-dictionary-alist
        '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)
          ("cy_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "cy_GB") nil utf-8))))

(use-package flyspell
  :hook (text-mode . flyspell-mode))

(use-package wucuo
  :ensure t
  :hook (prog-mode . wucuo-start)
  :init
  (remove-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (setq wucuo-personal-font-faces-to-check
        '(font-lock-comment-face font-lock-doc-face)))

(provide 'init-spell)
;;; init-spell.el ends here
