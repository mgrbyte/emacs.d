;;; init-gui-frames.el --- GUI and frame configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup fonts, theme, and frame position
;;; Code:

;; Disable UI elements
(use-package menu-bar
  :config (menu-bar-mode 0))

(use-package tool-bar
  :config
  (tool-bar-mode 0)
  :bind (("C-c t" . tool-bar-mode)))

(use-package fringe
  :after mgrbyte
  :config
  (when (mgrbyte-display-is-graphical)
    (fringe-mode (quote (5 . 5)))))

;; Frame commands - frame-cmds is from EmacsWiki, not MELPA
;; Use built-in frame functions instead
(global-set-key (kbd "C-c f m") 'toggle-frame-maximized)
(global-set-key (kbd "C-c f f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c f o") 'other-frame)

;; Text scaling
(use-package text-scale-mode
  :bind (("C-c +" . text-scale-increase)
         ("C-c -" . text-scale-decrease)))

;; Powerline
(use-package powerline

  :config
  (setq-default powerline-default-separator 'wave))

;; Maximize frames by default (works better than hooks for multi-monitor)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Late initialization - runs after init.el fully loaded (daemon startup)
(add-hook 'emacs-startup-hook
          (lambda ()
            (add-to-list 'default-frame-alist (cons 'font mgrbyte-default-font))
            (load-theme 'abyss t)))

;; New client frames - runs for each emacsclient -c
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (when (display-graphic-p)
              (run-at-time 0.3 nil #'mgrbyte-frame-to-external-maximized))))

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
