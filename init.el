;;; init.el --- Init file for developement environment. -*- lexical-binding: t; coding: utf-8 -*-
;;; Commentary:
;;
;; Initialize Emacs configuration.
;;
;;; Code:

;; Setup package management (Cask)
(require 'cask "~/.cask/cask.el")
(require 'tls)
(load-library "url-handlers")

(declare-function
 #'mgrbyte-delete-trailing-blank-lines
 "~/.emacs.d/lisp/mgrbyte.el")

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
      '(("gnu" . 0)
	("melpa-stable" . 5)
	("melpa" . 10)))

;; (setq package-pinned-packages
;;       '((cider . "melpa-stable")))

(defun secure-https-setup ()
  "Set up https securely as per Glyph's recommendations:
https://glyph.twistedmatrix.com/2015/11/editor-malware.html"
  (let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile))))

(secure-https-setup)
(cask-initialize)
(setq package-enable-at-startup nil)
(package-initialize)

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (require 'clojure-mode)
  (require 'dash)
  (require 'dashboard)
  (require 'diminish)
  (require 'f)
  (require 'flycheck)
  (require 'helm)
  (require 'helm-files)
  (require 'helm-lib)
  (require 'helm-net)
  (require 'jedi)
  (require 'org)
  (require 'org-agenda)
  (require 'org-list)
  (require 'pallet)
  (require 'reftex)
  (require 'reftex-cite)
  (require 'reftex-index)
  (require 'tex-mode)
  (require 's))

(use-package async
  :functions async-byte-comp-get-allowed-pkgs)

(use-package company
  :bind (("C-c h" . company-quickhelp-manual-begin)
	 ("C-c c" . company-complete))
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (company-quickhelp-mode 1))

(use-package company-quickhelp)

(use-package dockerfile-mode
  :mode (("Dockerfile" . dockerfile-mode)))

(use-package helm-cider
  :ensure t
  :defer t
  :after cider-mode)

(use-package helm-cider-history
  :ensure t
  :defer t
  :after helm-cider
  :init
  (bind-key "C-l" #'helm-cider-history))

(use-package helm-config
  :bind (("C-c h" . helm-command-prefix)
	 ("C-x b" . helm-mini)
	 ("C-x f" . helm-find-files)
	 ("C-x C-r" . helm-recentf)
	 ("M-x" . helm-M-x))
  :preface
  (progn
    (require 'helm)
    (unbind-key "C-x c")
    (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
    (bind-key "C-e" #'recentf-edit-list helm-map)
    (bind-key "C-z" #'helm-select-action helm-map))
  :config
  ;; Don't ask to create new files.
  (setq helm-ff-newfile-prompt-p nil)
  ;; open helm buffer inside current window, not occupy whole other window
  (setq helm-split-window-in-side-p t)
  ;; move to end or beginning of source when reaching top or bottom of source.
  (setq helm-move-to-line-cycle-in-source t)
  ;; search for library in `require' and `declare-function' sexp.
  (setq helm-ff-search-library-in-sexp t)
  ;; scroll 8 lines other window using M-<next>/M-<prior>s
  (setq helm-scroll-amount 8)
  (setq helm-buffers-fuzzy-matching t
	helm-recentf-fuzzy-match t)
  (setq helm-ff-file-name-history-use-recentf t)
  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command
	  "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command
	  "ack-grep -H --no-group --no-color %e %p %f")))

(use-package org
  :ensure org
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
    (setq org-agenda-files
	  (f-entries "~/org" (apply-partially #'s-ends-with? ".org") t))
    (setq org-directory "~/org")
    (setq org-default-notes-file "~/org/refile.org")
    (setq org-use-effective-time t)
    (setq org-goto-interface 'outline org-goto-max-level 10)
    (setq org-startup-folded nil)
    (setq org-cycle-include-plain-lists 'integrate)
    (add-to-list 'org-speed-commands-user
		 '("x" org-todo "DONE"))
    (add-to-list 'org-speed-commands-user
		 '("y" org-todo-yesterday "DONE"))
    (add-to-list 'org-speed-commands-user
		 '("!" my/org-clock-in-and-track))
    (add-to-list 'org-speed-commands-user
		 '("s" call-interactively 'org-schedule))
    (add-to-list 'org-speed-commands-user
		 '("d" my/org-move-line-to-destination))
    (add-to-list 'org-speed-commands-user
		 '("i" call-interactively 'org-clock-in))
    (add-to-list 'org-speed-commands-user
		 '("o" call-interactively 'org-clock-out))
    (add-to-list 'org-speed-commands-user
		 '("$" call-interactively 'org-archive-subtree))
    ;; (bind-key "!" 'my/org-clock-in-and-track org-agenda-mode-map)
    (bind-key "C-c j" 'org-clock-goto) ;; jump to current task from anywhere
    (bind-key "C-c C-w" 'org-refile)
    (bind-key "C-c r" 'org-capture)
    (bind-key "C-c a" 'org-agenda)
    (bind-key "C-c l" 'org-store-link)
    (bind-key "C-c L" 'org-insert-link-global)
    (bind-key "C-c O" 'org-open-at-point-global)
    ;; (bind-key "<f9> <f9>" 'org-agenda-list)
    ;; (bind-key "<f9> <f8>" (lambda () (interactive) (org-capture nil "r")))
    (bind-key "C-TAB" 'org-cycle org-mode-map)
    (bind-key "C-c v" 'org-show-todo-tree org-mode-map)
    (bind-key "C-c C-r" 'org-refile org-mode-map)
    (bind-key "C-c R" 'org-reveal org-mode-map)
    (org-clock-persistence-insinuate)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)))
    (eval-after-load 'org-agenda
      '(bind-key "i" 'org-agenda-clock-in org-agenda-mode-map)))
  (add-hook 'org-clock-in-prepare-hook 'mgrbyte--org-mode-ask-effort))

(use-package perspective)

(use-package persp-projectile
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-sort-order 'recentf))

(use-package recentf
  :bind (("C-x r e" . recentf-edit-list)))

(use-package package
  :bind (("C-c C-l" . list-packages)))

(use-package helm :diminish helm-mode)

(use-package helm-projectile
  :config
  (projectile-mode)
  (helm-projectile-on)
  (persp-mode))

(use-package mgrbyte
  :ensure abyss-theme
  :load-path "lisp"
  :bind (("C-c f g" . find-grep-dired))
  :preface
  (defun mgrbyte/add-to-hooks (function mode-hooks)
    "Add FUNCTION to multiple modes MODE-HOOKS."
    (mapc (lambda (hook) (add-hook hook function)) mode-hooks))
  :config
  ;; Track changes to install packages with Cask
  (pallet-mode t)
  ;; Turn off UI elements
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
  (setq transient-mark-mode t)
  (setq column-number-mode t)
  (setq inhibit-startup-message t)
  (setq search-highlight t)
  (setq query-replace-highlight t)
  (global-linum-mode 0)

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

  ;; Tramp
  (setq tramp-default-method "ssh")
  (setq tramp-shell-prompt-pattern
	"^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*")

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

(use-package cider-mode
  :config
  (setq cider-repl-use-pretty-printing 't)
  (setq cider-repl-history-size 10000)
  (setq cider-repl-history-file
	(f-join  (getenv "HOME") ".cider-repl-history"))
  (setq sayid-inject-dependencies-at-jack-in nil)
  (setq cljr-inject-dependencies-at-jack-in nil))

(use-package conf-mode
  :mode (("\\.conf" . conf-mode)
         ("\\.cfg" . conf-mode)
         ("\\.ini" . conf-mode)))

(use-package css-mode
  :mode (("\\.kss$" . css-mode)
         ("\\.css.dtml$". css-mode)))

(use-package dashboard
  :preface
  (defun dashboard-setup-startup-hook ()
    "Setup post initialization hooks.
If a command line argument is provided,
assume a filename and skip displaying Dashboard"
    (progn
      (add-hook 'after-init-hook (lambda ()
				   ;; Display useful lists of items
				   (dashboard-insert-startupify-lists)))
      (add-hook 'emacs-startup-hook '(lambda ()
				       (switch-to-buffer "*dashboard*")
				       (goto-char (point-min))
				       (redisplay)))))
  :init
  (get-buffer-create "*dashboard*")
  :config
  (setq-default dashboard-items '((projects . 10)
  				  (recents . 10)
  				  (bookmarks . 10)))
  (dashboard-setup-startup-hook))

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
  (fringe-mode (quote (4 . 0)))
  (eval-after-load 'cider '(flycheck-clojure-setup))
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))
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
    (setq flycheck-flake8-maximum-line-length 79)
    (setq flycheck-highlighting-mode 'lines)
    (advice-add 'flycheck-select-checker
		:after 'pyautomagic--remember-flycheck-checker)
    (advice-add 'flycheck-mode-line-status-text
		:around #'mgrbyte/flycheck-checker-name-on-mode-line)))

(use-package flycheck-clojure
  :ensure t
  :defer t
  :after flycheck)

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
		     markdown-mode-hook
		     message-mode-hook
		     org-mode-hook
		     rst-mode-hook
		     sphinx-doc-mode-hook)))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :config
  (setq-default indent-tabs-mode nil))

(use-package json-mode
  :mode (("\\.json$" . json-mode)))

(use-package java-mode
  :mode (("\\.java$" . java-mode)))

(use-package jedi
  :config
  (setq jedi:complete-on-dot t)
  (add-hook 'python-mode-hook #'jedi:setup))

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
  (unbind-key "M-s" paredit-mode-map)
  (unbind-key "M-r" paredit-mode-map)
  (mgrbyte/add-to-hooks
   #'enable-paredit-mode `(clojure-mode-hook emacs-lisp-mode-hook lisp-mode-hook)))

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
(use-package clj-refactor)

(use-package clojure-mode
  :preface
  (defun mgrbyte-setup-clj ()
    (clj-refactor-mode 1)
    (helm-cider-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2))
  (setq-default cljr-favor-prefix-notation t)
  (add-hook #'clojure-mode-hook #'mgrbyte-setup-clj))

(use-package python
  :bind (("C-c d i" . py-insert-debug)
	 ("RET" . newline-and-indent))
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

(use-package virtualenvwrapper
  :bind (("C-c w o" . venv-workon)
	 ("C-c w d" . venv-deactivate))
  :preface
  (defun mgrbyte/auto-activate-venv ()
    (hack-local-variables)
    (when (boundp 'mgrbyte-project-venv-name)
      (venv-workon mgrbyte-project-venv-name)))
  :config
  (mgrbyte/add-to-hooks
   #'mgrbyte/auto-activate-venv
   `(python-mode-hook
     rst-mode-hook))
  (setq-default mode-line-format
		(append
		 mode-line-format
		 '(:exec venv-current-name))))

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

(use-package tex-mode
  :preface
  (defvar reftex-index-phrase-mode)
  (defun turn-on-outline-minor-mode ()
    "Turn on the outline minor mode."
    (outline-minor-mode 1)
    (add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
    ;; (add-hook 'Latex-mode-hook 'turn-on-outline-minor-mode)
    (setq outline-minor-mode-prefix "C-c C-o"))
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
  (autoload #'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
  ;; (add-hook #'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook #'LaTeX-mode-hook #'turn-on-reftex))

(use-package text
  :mode (("\\.po$" . text-mode)
	 ("\\.pot$" . text-mode)))

(use-package text-scale-mode
  :bind (("C-c +" . text-scale-increase)
	 ("C-c -" . text-scale-decrease)))

(use-package whitespace-cleanup-mode
  :bind (("C-c _ w" . whitespace-mode)
	 ("C-c _ t" . whitespace-toggle-options)
	 ("C-c = w" . global-whitespace-mode)
	 ("C-c = t" . global-whitespace-toggle-options))
  :config
  (setq-default whitespace-cleanup-mode-preserve-point 't)
  (setq-default whitespace-cleanup-mode-only-if-initially-clean 't)
  (setq-default whitespace-cleanup-mode-ignore-modes
		'(cider-mode
		  cider-repl-mode
		  comint-mode
		  haskell-interactive-mode
		  helm-cider-mode
		  helm-mode
		  special-mode
		  view-mode)))

(use-package vcl
  :mode (("\\.vcl" . vcl-mode)))

(use-package yaml-mode
  :mode (("\\.yml" . yaml-mode)
	 ("\\.yaml" . yaml-mode)))

;; Ensure PATH is preserved from shell.
(exec-path-from-shell-initialize)

;; Follow links without asking
(setq-default vc-follow-symlinks 't)

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

(provide 'init)
;;; init.el ends here
