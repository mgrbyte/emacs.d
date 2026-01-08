;;; init-spell.el --- Spell checking with hunspell -*- lexical-binding: t -*-
;;; Commentary:
;; Configure hunspell for spell checking with Welsh and English dictionaries
;;; Code:

(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_GB")

;; Define available dictionaries
(setq ispell-local-dictionary-alist
      '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)
        ("cy_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "cy_GB") nil utf-8)))

;; Enable flyspell in text modes
(add-hook 'text-mode-hook #'flyspell-mode)

;; Enable flyspell for comments in prog modes
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(provide 'init-spell)
;;; init-spell.el ends here
