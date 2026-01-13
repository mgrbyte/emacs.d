;;; init-modes.el --- Miscellaneous language modes -*- lexical-binding: t -*-
;;; Commentary:
;; Setup various language and file modes
;;; Code:

;; Dockerfile
(use-package dockerfile-mode

  :mode (("Dockerfile" . dockerfile-mode)))

;; Config files
(use-package conf-mode
  :mode (("\\.conf" . conf-mode)
         ("\\.cfg" . conf-mode)
         ("\\.ini" . conf-mode)))

;; CSS
(use-package css-mode
  :mode (("\\.css$" . css-mode)))

;; JavaScript
(use-package js2-mode

  :mode (("\\.js$" . js2-mode))
  :config
  (setq-default indent-tabs-mode nil))

;; JSON
(use-package json-mode

  :mode (("\\.json$" . json-mode)))

;; Java
(use-package java-mode
  :mode (("\\.java$" . java-mode)))

;; Jinja2/HTML templates
(use-package jinja2-mode

  :mode (("\\.jinja$" . jinja2-mode)
         ("\\.html$" . jinja2-mode))
  :config
  (setq sgml-basic-offset 2))

;; Markdown
(use-package markdown-mode
  :mode (("\\.md$" . markdown-mode))
  :config
  (setq markdown-command "pandoc"))

;; Nix
(use-package nix-mode)

;; XML/ZCML
(use-package nxml-mode
  :mode (("\\.xml$" . nxml-mode)
         ("\\.zcml$" . nxml-mode))
  :config
  (add-hook 'nxml-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil))))

;; RST
(use-package rst
  :mode (("\\.rst$" . rst-mode))
  :config
  (setq fill-column 79)
  (setq rst-slides-program "google-chrome")
  (setq rst-adornment-faces-alist
        (quote ((nil . font-lock-keyword-face)
                (nil . font-lock-keyword-face)
                (nil . rst-level-1-face)
                (2 . rst-level-2-face)
                (3 . rst-level-3-face)
                (4 . rst-level-4-face)
                (5 . rst-level-5-face)
                (nil . rst-level-5-face))))
  :init
  (auto-fill-mode t))

;; Sass
(use-package sass-mode

  :config (setq sass-indent-offset 2))

;; SGML/Page Templates
(use-package sgml-mode
  :mode (("\\.pt$" . sgml-mode)
         ("\\.cpt$" . sgml-mode))
  :config
  (setq sgml-basic-offset 2)
  (add-hook 'sgml-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil))))

;; SQL
(use-package sql-mode
  :mode (("\\.zsql$" . sql-mode)
         ("\\.sql$" . sql-mode)))

;; Text/PO files
(use-package text-mode
  :mode (("\\.po$" . text-mode)
         ("\\.pot$" . text-mode)))

;; VCL (Varnish)
(use-package vcl-mode

  :mode (("\\.vcl" . vcl-mode)))

;; YAML
(use-package yaml-mode

  :mode (("\\.yml" . yaml-mode)
         ("\\.yaml" . yaml-mode)))

;; Emacs Lisp
(use-package emacs-lisp-mode
  :mode (("*scratch*" . emacs-lisp-mode)
         ("\\.el$" . emacs-lisp-mode)))

;; LaTeX
(use-package tex-mode
  :preface
  (defvar reftex-index-phrase-mode)
  (defun turn-on-outline-minor-mode ()
    "Turn on the outline minor mode."
    (outline-minor-mode 1)
    (setq outline-minor-mode-prefix "C-c C-o"))
  :hook ((LaTeX-mode . turn-on-outline-minor-mode)
         (LaTeX-mode . turn-on-reftex))
  :config
  (setq-default
   LaTeX-eqnarray-label "eq"
   LaTeX-equation-label "eq"
   LaTeX-figure-label "fig"
   LaTeX-myChapter-label "chap"
   LaTeX-section-hook '(LaTeX-section-heading
                        LaTeX-section-title
                        LaTeX-section-toc
                        LaTeX-section-section
                        LaTeX-section-label)
   LaTeX-table-label "tab"
   TeX-auto-save t
   TeX-newline-function #'reindent-then-newline-and-indent
   TeX-parse-self t
   Tex-save-query nil)
  (autoload #'reftex-mode "reftex" "RefTeX Minor Mode" t)
  (autoload #'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
  (autoload #'reftex-citation "reftex-cite" "Make citation" nil)
  (autoload #'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t))

(provide 'init-modes)
;;; init-modes.el ends here
