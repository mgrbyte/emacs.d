;;; init.el --- Init file for developement environment. -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;
;; Initialize Emacs configuration.
;;
;;; Code:

;; Package management with package.el
(require 'package)
(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
  '(("gnu" . 0)
    ("melpa-stable" . 5)
    ("melpa" . 10)))

(package-initialize)

;; Install use-package if not present
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; Don't auto-ensure - we'll specify per package
;; Built-in packages don't need :ensure

;; macOS: swap Command/Option - Command is Meta, Option passes through
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier nil))

;; Start on external monitor (right of laptop) and maximized
(setq initial-frame-alist '((left . 1728) (top . 6) (fullscreen . maximized)))

(declare-function
 #'mgrbyte-delete-trailing-blank-lines
 "~/.emacs.d/lisp/mgrbyte.el")

(use-package async
  :functions async-byte-comp-get-allowed-pkgs)

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dash :ensure t)
(use-package f :ensure t)
(use-package s :ensure t)

(use-package auth-source
  :after dash
  :config
  (setq auth-source-debug t)
  (setq auth-sources (-filter #'file-exists-p '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))))

(use-package dockerfile-mode
  :mode (("Dockerfile" . dockerfile-mode)))

(use-package helm
  :ensure t
  :bind (("C-c h" . helm-command-prefix)
	 ("C-x b" . helm-mini)
	 ("C-x f" . helm-find-files)
	 ("C-x C-r" . helm-recentf)
	 ("M-x" . helm-M-x))
  :init
  (unbind-key "C-x c")
  :config
  (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
  (bind-key "C-e" #'recentf-edit-list helm-map)
  (bind-key "C-z" #'helm-select-action helm-map)
  ;; Don't ask to create new files.
  (setq helm-ff-newfile-prompt-p nil)
  ;; open helm buffer inside current window, not occupy whole other window
  (setq helm-split-window-inside-p t)
  ;; move to end or beginning of source when reaching top or bottom of source.
  (setq helm-move-to-line-cycle-in-source t)
  ;; search for library in `require' and `declare-function' sexp.
  (setq helm-ff-search-library-in-sexp t)
  ;; scroll 8 lines other window using M-<next>/M-<prior>s
  (setq helm-scroll-amount 8)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-ff-file-name-history-use-recentf t)
  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command
	  "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command
	  "ack-grep -H --no-group --no-color %e %p %f")))

(use-package org
  :ensure t
  :after (f s)
  :config
  (progn
    (defun mgrbyte--org-use-speed-commands-for-headings-and-lists ()
      "Activate speed commands on list items too."
      (or (and (looking-at org-outline-regexp) (looking-back "^\**" 100))
	  (save-excursion
	    (and (looking-at (org-item-re)) (looking-back "^[ \t]*" 100)))))

    (defun mgrbyte--org-mode-ask-effort ()
      "Ask for an effort estimate when clocking in."
      (require 'org)
      (unless (org-entry-get (point) "Effort")
	(let ((effort
	       (completing-read
		"Effort: "
		(org-entry-get-multivalued-property (point) "Effort"))))
	  (unless (equal effort "")
	    (org-set-property "Effort" effort)))))

    (setq org-log-done 'time)
    (setq org-todo-keywords
	  (quote ((sequence
		   "TODO(t)"
		   "NEXT(n)"
		   "STARTED(s)"
		   "|"
		   "DONE(d)")
		  (sequence
		   "WAITING(w@/!)"
		   "HOLD(h@/!)"
		   "|"
		   "CANCELLED(c@/!)"
		   "PHONE"
		   "MEETING"))))
    (setq org-default-notes-file "~/org/notes.org")
    (setq org-agenda-files (f-entries "~/org" (apply-partially #'s-ends-with? ".org") t))
    (setq org-directory "~/org")
    (setq org-use-effective-time t)
    (setq org-startup-folded t)
    (setq org-cycle-include-plain-lists 'integrate)
    (add-to-list 'org-speed-commands
		 '("x" org-todo "DONE"))
    (add-to-list 'org-speed-commands
		 '("y" org-todo-yesterday "DONE"))
    (add-to-list 'org-speed-commands
		 '("!" my/org-clock-in-and-track))
    (add-to-list 'org-speed-commands
		 '("s" call-interactively 'org-schedule))
    (add-to-list 'org-speed-commands
		 '("d" my/org-move-line-to-destination))
    (add-to-list 'org-speed-commands
		 '("i" call-interactively 'org-clock-in))
    (add-to-list 'org-speed-commands
		 '("o" call-interactively 'org-clock-out))
    (add-to-list 'org-speed-commands
		 '("$" call-interactively 'org-archive-subtree))
    ;; (bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map)
    (bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere
    (bind-key "C-c C-w" 'org-refile)
    (bind-key "C-c r" 'org-capture)
    (bind-key "C-c a" 'org-agenda)
    (bind-key "C-c l" 'org-store-link)
    (bind-key "C-c L" 'org-insert-link-global)
    (bind-key "C-c O" 'org-open-at-point-global)
    (bind-key "C-TAB" 'org-cycle org-mode-map)
    (bind-key "C-c v" 'org-show-todo-tree org-mode-map)
    (bind-key "C-c C-r" 'org-refile org-mode-map)
    (bind-key "C-c R" 'org-reveal org-mode-map)
    (org-clock-persistence-insinuate)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (clojure . t)))
    (eval-after-load 'org-agenda
      '(bind-key "i" 'org-agenda-clock-in org-agenda-mode-map)))
  (add-hook 'org-clock-in-prepare-hook 'mgrbyte--org-mode-ask-effort))

;; perspective removed - incompatible with Emacs 30

(use-package projectile
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-sort-order 'recentf))

(use-package treemacs
  :ensure t
  :bind (("C-c e" . treemacs)
         ("C-c E" . treemacs-select-window))
  :config
  (setq treemacs-width 35)
  (setq treemacs-position 'right))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package recentf
  :bind (("C-x r e" . recentf-edit-list))
  :config
  ;; Don't keep history of Cask updates in recent-f
  (add-to-list 'recentf-exclude (expand-file-name "~/git/emacs.d/.cask.*")))

(use-package package
  :bind (("C-c C-l" . list-packages)))

(use-package helm :diminish helm-mode)

(use-package helm-projectile
  :bind (("C-x f" . projectile-find-file)
         ("C-x p g" . projectile-grep))
  :config
  (projectile-mode))

(use-package mgrbyte
  :load-path "lisp"
  :bind (("C-x t w" . delete-trailing-whitespace)
         ("C-c f g" . find-grep-dired))
  :config
  ;; encoding
  (mgrbyte-configure-encoding 'utf-8)

  ;; VC
  ;; Follow links without asking
  (setq-default vc-follow-symlinks 't)
  (setq vc-handled-backends '(Git))

  ;; Misc settings.
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

  ;; Desktop mode
  ;; Useful for remembering a set of file you're working on -
  ;;  - enables switching between projects and keeping state.
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
  (bind-key "C-c t" #'tool-bar-mode)

  ;; avoid audio beeping by turning on visible-bell
  (setq visible-bell nil)
  (setq debug-on-error t)
  (setq custom-theme-directory (locate-user-emacs-file "themes"))
  (setq-default theme-load-from-file t)
  (add-to-list 'auto-mode-alist '("Makfile.*" . makefile-gmake-mode))
  (keyfreq-mode)
  (menu-bar-mode 0)
  (helm-mode 1))

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-default-remote-shell "/bin/zsh")
  (setq tramp-encoding-shell "/bin/zsh")
  (setq tramp-shell-prompt-pattern
	"^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"))

(use-package bookmark
  :config
  (define-key global-map [menu-bar bookmarks]
    (cons "Bookmarks" (make-sparse-keymap "Bookmarks")))
  (define-key global-map
    [menu-bar bookmarks bookmark-insert]
    '("Insert bookmark into buffer" . bookmark-insert))
  (define-key global-map
    [menu-bar bookmarks bookmark-delete]
    '("Delete bookmark" . bookmark-delete))
  (define-key global-map
    [menu-bar bookmarks bookmark-save]
    '("Save bookmarks" . bookmark-save))
  (define-key global-map
    [menu-bar bookmarks list-bookmarks]
    '("List bookmarks" . list-bookmarks))
  (define-key global-map
    [menu-bar bookmarks bookmark-set]
    '("Add bookmark" . bookmark-sebt))
  (define-key global-map
    [menu-bar bookmarks bookmark-jump]
    '("Goto bookmark" . bookmark-jump)))

(use-package conf-mode
  :mode (("\\.conf" . conf-mode)
         ("\\.cfg" . conf-mode)
         ("\\.ini" . conf-mode)))

(use-package css-mode
  :mode (("\\.css$" . css-mode)))

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-item-shortcuts '((agenda . "a")
                                   (recents . "r")
                                   (projects . "p")
                                   (bookmarks . "m")))
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-projects-switch-function 'projectile-switch-project)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)))
  (dashboard-setup-startup-hook))

(use-package dired
  :config
  (advice-add 'dired-readin :after #'mgrbyte-sort-directories-first))

(use-package dired-x
  :config
  (setq-default dired-omit-files-p nil)
  (setq dired-omit-files
	(concat dired-omit-files "\\|^\\..+$")))

(use-package ediff
  :config
  (setq ediff-shell (getenv "$SHELL"))
  (setq-default ediff-split-window-function
		(quote split-window-vertically)))

(use-package vc
  :config
  (defun my-vc-diff-side-by-side (orig-fun &rest args)
    "Run vc-diff with side-by-side split."
    (let ((split-height-threshold nil)
          (split-width-threshold 0))
      (apply orig-fun args)))
  (advice-add 'vc-diff :around #'my-vc-diff-side-by-side))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package emacs-lisp-mode
  :mode (("*scratch*" . emacs-lisp-mode)
         ("\\.el$" . emacs-lisp-mode)))

(use-package erc)

(use-package flymake :disabled t)

(use-package frame-cmds
  :bind (("C-c f m" . maximize-frame)
	 ("C-c f r" . restore-frame)
	 ("C-c f o" . other-window-or-frame)
	 ("<M-up>" . move-frame-up)
	 ("<M-down>" . move-frame-down)
	 ("<M-left>" . move-frame-left)
 	 ("<M-right>" . move-frame-right)))

(use-package fringe
  :after (mgrbyte)
  :config
  (when (mgrbyte-display-is-graphical)
    (fringe-mode (quote (5 . 5)))))

(use-package menu-bar
  :after (mgrbyte scroll-bar)
  :config
  ;; Turn off UI elements
  (mapc #'apply `((menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)))
  ;; setup frame
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'mgrbyte-setup-frame)
    (mgrbyte-setup-frame)))

(use-package gist)

(use-package gnus
  :bind (("C-x g" . gnus-other-frame)))

(use-package google-this)

(use-package ispell
  :bind (("C-c i" . ispell-buffer))
  :config
  (mapc (lambda (envvar) (setenv envvar "en_GB.UTF8")) '("LANG"))
  (setq-default ispell-program-name "aspell")
  (setq ispell-dictionary "cy")
  (ispell-set-spellchecker-params)
  ;; (ispell-hunspell-add-multi-dic langs)
  (setq ispell-personal-dictionary "~/.hunspell_personal"))

(use-package jinja2-mode
  :mode (("\\.jinja$" . jinja2-mode)
          ("\\.html$" . jinja2-mode))
  :config
  (setq sgml-basic-offset 2))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :config
  (setq-default indent-tabs-mode nil))

(use-package json-mode
  :mode (("\\.json$" . json-mode)))

(use-package java-mode
  :mode (("\\.java$" . java-mode)))

;; (use-package jedi
;;   :config
;;   (setq jedi:complete-on-dot t))

(use-package keyfreq)

(use-package ls-lisp
  :config (setq ls-lisp-use-insert-directory-program nil))

(use-package magit
  :bind (("C-c m" . magit-status))
  :config
  (defun my-magit-side-by-side (orig-fun &rest args)
    "Run magit-status with side-by-side split."
    (let ((split-height-threshold nil)
          (split-width-threshold 0))
      (apply orig-fun args)))
  (advice-add 'magit-status :around #'my-magit-side-by-side))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md$" . markdown-mode)))

(use-package mule
  :after (mgrbyte)
  :config
  (mgrbyte-configure-encoding 'utf-8))

(use-package notify
  :load-path "lisp")

(use-package nxml-mode
  :mode (("\\.xml$" . nxml-mode)
         ("\\.zcml$" . nxml-mode))
  :config
  (add-hook 'nxml-mode-hook
	    (lambda ()
	      (setq indent-tabs-mode nil))))

(use-package paredit
  :diminish paredit-mode
  :init
  (enable-paredit-mode)
  :config
  (unbind-key "M-s" paredit-mode-map)
  (unbind-key "M-r" paredit-mode-map))

(use-package paren
  :config
  (setq show-paren-style 'expression)
  :init
  (show-paren-mode 1))

(use-package powerline
  :config
  (setq-default powerline-default-separator 'wave))

(use-package clojure-mode
  :preface
  (defun mgrbyte-setup-clj ()
    (yas-minor-mode 1))
  :hook (mgrbyte-setup-clj)
  :config
  (require 'flycheck-clj-kondo)
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

(use-package python
  :bind (("C-c d i" . py-insert-debug)
	       ("RET" . newline-and-indent))
  :mode (("\\.py$" . python-mode)
         ("\\.cpy$" . python-mode)
         ("\\.vpy$" . python-mode)
         ("\\.html$" . jinja2-mode))
  :config
  (declare-function py-insert-debug mgrbyte nil)
  (setq fill-column 79)
  (setq tab-width 4))

(use-package python-pytest
  :ensure t
  :bind (:map python-mode-map
         ("C-c p p" . python-pytest-dispatch)
         ("C-c p f" . python-pytest-file)
         ("C-c p ." . python-pytest-run-def-or-class-at-point)
         ("C-c p r" . python-pytest-repeat)
         ("C-c p l" . python-pytest-last-failed))
  :config
  (setq python-pytest-executable "uv run pytest"))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((python-mode . lsp)
         (python-mode . (lambda ()
                          (add-hook 'before-save-hook #'lsp-organize-imports nil t)
                          (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
  :bind (:map lsp-mode-map
         ("C-c f" . lsp-format-buffer)
         ("C-c o" . lsp-organize-imports))
  :config
  (setq lsp-auto-guess-root t)
  ;; Disable ruff LSP server (we use python-lsp-ruff plugin in pylsp instead)
  (add-to-list 'lsp-disabled-clients 'ruff)
  (setq lsp-pylsp-plugins-ruff-enabled t)
  (setq lsp-pylsp-plugins-ruff-format-enabled t)
  (setq lsp-pylsp-plugins-mypy-enabled t)
  (setq lsp-pylsp-plugins-mypy-live-mode t)    ; run live as you type
  (setq lsp-pylsp-plugins-mypy-dmypy nil)      ; don't use daemon (avoids cache issues)
  ;; Disable built-in linters (ruff handles these)
  (setq lsp-pylsp-plugins-pycodestyle-enabled nil)
  (setq lsp-pylsp-plugins-pyflakes-enabled nil)
  (setq lsp-pylsp-plugins-mccabe-enabled nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
         ("C-?" . lsp-ui-doc-glance))
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point))

(use-package pyautomagic
  :load-path "lisp"
  :bind (("C-c v e" . pyautomagic--activate-venv-safely)
	 ("C-c f c" . pyautomagic--configureq-flycheck-checkers)))

(use-package py-snippets
  :ensure t
  :after yasnippet
  :config
  (py-snippets-initialize))

(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode-enable))

(use-package rst
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
  (auto-fill-mode t)
  :mode (("\\.rst$" . rst-mode)))

(use-package sass-mode
  :config (setq sass-indent-offset 2))

;; Emacs server configuration
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

(use-package sgml-mode
  :config
  (setq sgml-basic-offset 2)
  (add-hook 'sgml-mode-hook
	    (lambda ()
	      (setq indent-tabs-mode nil)))
  :mode (("\\.pt$" . sgml-mode)
         ("\\.cpt$" . sgml-mode)))

(use-package shell
  :config (setq shell-prompt-pattern "\\u@\\h: \\w $ "))

(use-package sql-mode
  :mode (("\\.zsql$" . sql-mode)
         ("\\.sql$" . sql-mode)))

(use-package tex-mode
  :preface
  (defvar reftex-index-phrase-mode)
  (defun turn-on-outline-minor-mode ()
    "Turn on the outline minor mode."
    (outline-minor-mode 1)
    (setq outline-minor-mode-prefix "C-c C-o"))
  :hook ((turn-on-outline-minor-mode turn-on-reftex))
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
   TeX-auto-save t
   TeX-newline-function #'reindent-then-newline-and-indent
   TeX-parse-self t
   TeX-parse-self t
   Tex-save-query nil)
  (autoload #'reftex-mode "reftex" "RefTeX Minor Mode" t)
  (autoload #'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
  (autoload #'reftex-citation "reftex-cite" "Make citation" nil)
  (autoload #'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t))

(use-package text
  :mode (("\\.po$" . text-mode)
	       ("\\.pot$" . text-mode)))

(use-package text-scale-mode
  :bind (("C-c +" . text-scale-increase)
	 ("C-c -" . text-scale-decrease)))

(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  :ensure t
  :bind (("C-c v" . vterm-toggle)
         ("C-c V" . vterm-toggle-cd))
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project))

(use-package whitespace-cleanup-mode
  :bind (("C-c _ w" . whitespace-mode)
	 ("C-c _ t" . whitespace-toggle-options)
	 ("C-c = w" . global-whitespace-mode)
	 ("C-c = t" . global-whitespace-toggle-options))
  :config
  (setq-default whitespace-cleanup-mode-preserve-point 't)
  (setq-default whitespace-cleanup-mode-only-if-initially-clean 't)
  (setq-default whitespace-cleanup-mode-ignore-modes
		'(helm-mode
		  special-mode
		  view-mode)))

(use-package vcl
  :mode (("\\.vcl" . vcl-mode)))

(use-package yaml-mode
  :mode (("\\.yml" . yaml-mode)
	 ("\\.yaml" . yaml-mode)))

;; Ensure PATH is preserved from shell.
;; Use login shell only (not interactive) to avoid slow startup.

(setq exec-path-from-shell-arguments '("-l"))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;; custom user Lisp (from template on first load)
(defvar user-custom-file (f-expand "~/.emacs-custom.el"))
(unless (f-exists? user-custom-file)
  (with-current-buffer (get-buffer-create "user-custom-file")
    (insert-file-contents
     (locate-user-emacs-file "user-custom-file-template.el") nil 0)
     (write-region (buffer-string) nil user-custom-file)))

(if (f-exists? user-custom-file)
    (load user-custom-file))

;; Code generated by using the Emacs *customize* interfaces goes to its own file.
(setq custom-file (f-expand "~/.emacs-customize.el"))
(if (f-exists? custom-file)
  (load custom-file))

;;; Load custom theme
(load-theme 'tango-dark 'no-confirm)

(provide 'init)
;;; init.el ends here
