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
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_GB")
  (setq ispell-local-dictionary-alist
        '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)
          ("cy_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "cy_GB") nil utf-8))))

(use-package flyspell
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(provide 'init-spell)
;;; init-spell.el ends here
