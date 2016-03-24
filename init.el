;;; init.el --- Init file for developement environment. -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;
;; Initialize Emacs configuration.
;;
;;; Code:

;; Setup package management (Cask)
(require 'cask "~/.cask/cask.el")

(cask-initialize)
(setq package-enable-at-startup nil)
(package-initialize)

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (require 'dash)
  (require 'diminish)
  (require 'f)
  (require 'flycheck)
  (require 'pallet)
  (require 's))

(use-package async
  :functions async-byte-comp-get-allowed-pkgs)

(use-package mgrbyte
  :ensure abyss-theme
  :load-path "lisp"
  :preface
  (defun mgrbyte/add-to-hooks (function mode-hooks)
    "Add FUNCTION to multiple modes MODE-HOOKS."
    (mapc (lambda (hook) (add-hook hook function)) mode-hooks))
  :config
  (mapc #'apply
	`((menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1)))

  ;; Misc settings.
  (setq-default indent-line-function 'insert-tab)
  (setq indent-tabs-mode nil)
  (setq tab-always-indent nil)

  ;; Scrolling - do not add newlines when cursoring past last line in file
  (setq scroll-step 1)
  (setq next-line-add-newlines nil)

  ;; Display
  (global-linum-mode 0)
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
  (setq visible-bell t)
  (setq debug-on-error t)
  (setq custom-theme-directory (locate-user-emacs-file "themes"))
  (setq-default theme-load-from-file t)
  (add-to-list 'auto-mode-alist '("Makfile.*" . makefile-gmake-mode))
  (keyfreq-mode)
  (menu-bar-mode 0)
  (helm-mode 1)
  (load-theme 'abyss t))

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
  :mode (("\\.kss$" . css-mode)
         ("\\.css.dtml$". css-mode)))

(use-package dired
  :config
  (defadvice dired-readin
      (after dired-after-updating-hook first () activate)
    "Sort dired listings with directories first before adding mark."
    (mgrbyte-sort-directories-first)))

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

(use-package editorconfig)

(use-package emacs-lisp-mode
  :mode (("*scratch*" . emacs-lisp-mode)
	 ("\\.el$" . emacs-lisp-mode)))

(use-package erc)

(use-package flycheck
  :preface
  (declare-function flycheck-next-error flycheck nil)
  (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
  (fringe-mode (quote (4 . 0)))
  (eval-after-load 'flycheck '(flycheck-clojure-setup))
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (global-flycheck-mode 1)
  (defun mgrbyte/flycheck-checker-name-on-mode-line (oldfun &optional status)
    "Show the current checker name using OLDFUN and STATUS.

Result will be shown in the flycheck mode-line."
    (let ((res (apply oldfun status)))
      ;; Unless there is no current checker
      (if flycheck-checker
	  (s-replace "FlyC" (format "FlyC[%s]" flycheck-checker) res)
	res)))
  :config
  (progn
    (setq flycheck-emacs-lisp-load-path 'inherit)
    (setq flycheck-python-flake8-executable "flake8")
    (setq flycheck-flake8-maximum-line-length 79)
    (setq flycheck-highlighting-mode 'lines)
    (advice-add 'flycheck-select-checker
		:after 'pyautomagic--remember-flycheck-checker)
    (advice-add 'flycheck-mode-line-status-text
		:around #'mgrbyte/flycheck-checker-name-on-mode-line)))

;; Used for constributing 3rd party python packages
;; instead of the more imposing flycheck-flake8 checker
;;; (which is the default for my own and work packages)
(use-package flycheck-pyflakes)

(use-package flymake :disabled t)

(use-package frame-cmds
  :bind (("C-c f m" . maximize-frame)
	 ("C-c f r" . restore-frame)
	 ("C-c f o" . other-window-or-frame)
	 ("<M-up>" . move-frame-up)
	 ("<M-down>" . move-frame-down)
	 ("<M-left>" . move-frame-left)
 	 ("<M-right>" . move-frame-right)))

(use-package gist)

(use-package git-gutter+)

(use-package gnus
  :bind (("C-x g" . gnus-other-frame)))

(use-package google-this)

(use-package ispell
  :bind (("C-c i" . ispell-buffer))
  :init
  (mgrbyte/add-to-hooks
   #'flyspell-mode `(LaTeX-mode-hook
		     git-commit-mode-hook
		     jabber-chat-mode-hook
		     markdown-mode-hook
		     message-mode-hook
		     org-mode-hook
		     rst-mode-hook
		     sphinx-doc-mode-hook)))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :config
  (setq-default indent-tabs-mode nil))

(use-package java-mode
  :mode (("\\.js.dtml$" . java-mode)))

(use-package jedi
  :bind (("C-." . jedi:goto-definition)
	 ("C-c r" . jedi:related-names)
	 ("C-?" . jedi:show-doc))
  :config
  (setq-default jedi:complete-on-dot t))

(use-package keyfreq)

(use-package ls-lisp
  :config (setq ls-lisp-use-insert-directory-program nil))

(use-package magit
  :bind (("C-c m" . magit-status)))

(use-package mardown-mode
  :mode (("\\.md$" . markdown-mode)))

(use-package mule
  :config (setq locale-coding-system 'utf-8)
  :init
  (set-language-environment 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8))

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
  :config
  (mgrbyte/add-to-hooks
   #'enable-paredit-mode `(cider-repl-mode-hook
			   clojure-mode-hook
			   emacs-lisp-mode-hook
			   lisp-mode-hook)))

(use-package paren
  :config
  (setq show-paren-style 'expression)
  :init
  (show-paren-mode 1))

(use-package powerline
  :config
  (setq-default powerline-default-separator 'wave))

(use-package pretty-symbols
  :diminish pretty-symbols-mode
  :preface
  (defun enable-pretty-symbols-mode ()
    (pretty-symbols-mode 1))
  :config
  (mgrbyte/add-to-hooks
   #'enable-pretty-symbols-mode `(emacs-lisp-mode-hook
				  lisp-mode-hook
				  clojure-mode-hook
				  python-mode-hook)))

(use-package python
  :bind (("<kp-5>" . py-insert-debug)
         ("<f9>" . py-insert-debug))
  :mode (("\\.py$" . python-mode)
         ("\\.cpy$" . python-mode)
         ("\\.vpy$" . python-mode))
  :config
  (declare-function py-insert-debug mgrbyte nil)
  (setq fill-column 79)
  (setq-default flycheck-flake8rc "~/.config/flake8rc")
  (setq python-check-command "flake8")
  (setq tab-width 4)
  (sphinx-doc-mode t))

(use-package pyautomagic
  :load-path "lisp"
  :bind (("C-c v e" . pyautomagic--activate-venv-safely)
	 ("C-c f c" . pyautomagic--configure-flycheck-checkers)))

(use-package pyvenv)

(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode-enable))

(use-package rst
  :config
  (setq fill-column 79)
  (setq rst-adornment-faces-alist
	(quote ((nil . font-lock-keyword-face)
		(nil . font-lock-keyword-face)
		(nil . rst-level-1-face)
		(2 . rst-level-2-face)
		(3 . rst-level-3-face)
		(4 . rst-level-4-face)
		(5 . rst-level-5-face)
		(nil . rst-level-5-face))))
  :mode (("\\.rst$" . rst-mode)))

(use-package sass-mode
  :config (setq sass-indent-offset 2))

;; Emacs server configuration
;; Allows use with screen
;; Start either gnuserv or emacsserver for external access
(use-package server
  :config
  (setq server-socket-dir
	(format "%semacs%d"
		temporary-file-directory
		(user-uid)))
  (setq server-use-tcp 't)
  :init
  (when (not (or
	      (window-system)
	      (eq 'windows-nt system-type)))
    (server-start)))

(use-package sgml-mode
  :config
  (setq sgml-basic-offset 4)
  (add-hook 'sgml-mode-hook
	    (lambda ()
	      (setq indent-tabs-mode nil)))
  :mode (("\\.pt$" . sgml-mode)
         ("\\.cpt$" . sgml-mode)
         ("\\.html" . sgml-mode)
         ("\\.htm" . sgml-mode)))

(use-package shell
  :config (setq shell-prompt-pattern "\\u@\\h: \\w $ "))

(use-package sql-mode
  :mode (("\\.zsql$" . sql-mode)
         ("\\.sql$" . sql-mode)))

(use-package text
  :mode (("\\.po$" . text-mode)
	 ("\\.pot$" . text-mode)))

(use-package text-scale-mode
  :bind (("C-c +" . text-scale-increase)
	 ("C-c -" . text-scale-decrease)))

(use-package vcl
  :mode (("\\.vcl" . vcl-mode)))

(use-package yaml-mode
  :mode (("\\.yml" . yaml-mode)
	 ("\\.yaml" . yaml-mode)))

;; Ensure PATH is preserved from shell.
(exec-path-from-shell-initialize)

;;; custom user Lisp (from template on first load)
(defvar user-custom-file "~/.emacs-custom.el")
(unless (file-exists-p user-custom-file)
  (with-current-buffer (get-buffer-create "user-custom-file")
    (insert-file-contents
     (locate-user-emacs-file "user-custom-file-template.el") nil 0)
     (write-region (buffer-string) nil user-custom-file)))

(load user-custom-file)

;; Code generated by using the Emacs *cusomize* interfaces goes to its own file.
(setq custom-file "~/.emacs-customize.el")
(load custom-file)

(provide 'init)
;;; init.el ends here
