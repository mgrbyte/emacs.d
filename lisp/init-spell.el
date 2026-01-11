;;; init-spell.el --- Spell checking with hunspell -*- lexical-binding: t -*-
;;; Commentary:
;; Configure hunspell for spell checking with Welsh and English dictionaries
;;; Code:

(use-package ispell
  :bind (("C-c s b" . ispell-buffer)
         ("C-c s w" . ispell-word)
         ("C-c s r" . ispell-region)
         ("C-c s d" . mgrbyte-ispell-change-dictionary))
  :config
  (defun mgrbyte-ispell-change-dictionary ()
    "Change dictionary and re-check buffer with flyspell."
    (interactive)
    (call-interactively #'ispell-change-dictionary)
    (when flyspell-mode
      (flyspell-buffer)))
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

  :hook (prog-mode . wucuo-start)
  :init
  (remove-hook 'prog-mode-hook #'flyspell-prog-mode)
  :config
  (setq wucuo-font-faces-to-check
        '(font-lock-comment-face font-lock-doc-face)))

(provide 'init-spell)
;;; init-spell.el ends here
