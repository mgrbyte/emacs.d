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
    (server-start))
  ;; Focus new frames when created via emacsclient
  (add-hook 'server-after-make-frame-hook
            (lambda () (select-frame-set-input-focus (selected-frame)))))

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
  (set-face-foreground 'vterm-color-black "#16161E")
  (set-face-foreground 'vterm-color-red "#F7768E")
  (set-face-foreground 'vterm-color-green "#41A6B5")
  (set-face-foreground 'vterm-color-yellow "#E0AF68")
  (set-face-foreground 'vterm-color-blue "#7AA2F7")
  (set-face-foreground 'vterm-color-magenta "#BB9AF7")
  (set-face-foreground 'vterm-color-cyan "#7DCFFF")
  (set-face-foreground 'vterm-color-white "#787C99")
  (set-face-foreground 'vterm-color-bright-black "#444B6A")
  (set-face-foreground 'vterm-color-bright-red "#F7768E")
  (set-face-foreground 'vterm-color-bright-green "#41A6B5")
  (set-face-foreground 'vterm-color-bright-yellow "#E0AF68")
  (set-face-foreground 'vterm-color-bright-blue "#7AA2F7")
  (set-face-foreground 'vterm-color-bright-magenta "#BB9AF7")
  (set-face-foreground 'vterm-color-bright-cyan "#7DCFFF")
  (set-face-foreground 'vterm-color-bright-white "#D5D6DB"))

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
  :hook (after-init . envrc-global-mode)
  :config
  ;; Fix envrc faces for abyss theme (mode-line contrast on skyblue background)
  (set-face-attribute 'envrc-mode-line-on-face nil
                      :inherit nil :foreground "#009e73" :weight 'bold)
  (set-face-attribute 'envrc-mode-line-error-face nil
                      :inherit nil :foreground "#FF1A00" :weight 'bold)
  (set-face-attribute 'envrc-mode-line-none-face nil
                      :inherit nil :foreground "#050000" :weight 'bold))

(provide 'init-misc)
;;; init-misc.el ends here
