;;; init-misc.el --- Miscellaneous packages -*- lexical-binding: t -*-
;;; Commentary:
;; Setup various utility packages
;;; Code:

;; Tramp
(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-default-remote-shell "/bin/zsh")
  (setq tramp-encoding-shell "/bin/zsh")
  (setq tramp-shell-prompt-pattern
        "^[^$>\n]*[#$%>] *\\(\[[0-9;]*[a-zA-Z] *\\)*"))

;; Shell
(use-package shell
  :config (setq shell-prompt-pattern "\\u@\\h: \\w $ "))

;; Bookmarks
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
    '("Add bookmark" . bookmark-set))
  (define-key global-map
    [menu-bar bookmarks bookmark-jump]
    '("Goto bookmark" . bookmark-jump)))

;; Emacs server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; ERC
(use-package erc
  :defer t)

;; Google search
(use-package google-this

  :defer t)

;; Gnus
(use-package gnus
  :defer t
  :bind (("C-x g" . gnus-other-frame)
         ("C-c C-x m" . gnus)))

;; Terminal
(use-package vterm
  :defer t
  :config
  (setq vterm-always-compile-module t)
  (setq vterm-max-scrollback 10000)
  ;; Tokyo Night Terminal Dark palette (matches Alacritty)
  (setq vterm-color-palette
        ["#16161E"   ; black (base00)
         "#F7768E"   ; red (base08)
         "#41A6B5"   ; green (base0B)
         "#E0AF68"   ; yellow (base0A)
         "#7AA2F7"   ; blue (base0D)
         "#BB9AF7"   ; magenta (base0E)
         "#7DCFFF"   ; cyan (base0C)
         "#787C99"   ; white (base05)
         "#444B6A"   ; bright black (base03)
         "#F7768E"   ; bright red
         "#41A6B5"   ; bright green
         "#E0AF68"   ; bright yellow
         "#7AA2F7"   ; bright blue
         "#BB9AF7"   ; bright magenta
         "#7DCFFF"   ; bright cyan
         "#D5D6DB"]) ; bright white (base07)
  )

(use-package vterm-toggle

  :bind (("C-c v" . vterm-toggle)
         ("C-c V" . vterm-toggle-cd))
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'project))

;; Package management UI
(use-package package
  :bind (("C-c C-l" . list-packages)))

;; Encoding
(use-package mule
  :after mgrbyte
  :config
  (mgrbyte-configure-encoding 'utf-8))

;; Reload dir-locals
(use-package reload-dir-locals
  :load-path "lisp"
  :bind (("C-c d l r" . reload-dir-locals-for-current-buffer)))

;; Notifications
(use-package notify
  :load-path "lisp")

(use-package envrc
  :hook (after-init . envrc-global-mode))

(provide 'init-misc)
;;; init-misc.el ends here
