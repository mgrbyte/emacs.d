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
  :ensure t
  :config
  (setq-default powerline-default-separator 'wave))

;; Late initialization - runs after init.el fully loaded
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (window-system)
              (set-face-attribute 'default nil :font
                                  (if (eq system-type 'darwin) "Menlo 14" "Ubuntu Mono 14"))
              (set-frame-position (selected-frame) 1728 6)
              (toggle-frame-maximized))
            (load-theme 'abyss t)))

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
