;;; init-gui-frames.el --- GUI and frame configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup fonts, theme, and frame position
;;; Code:

;; Allow pixel-level frame resizing so tiling fills the full work area
;; without gaps from character-row boundary snapping (safe on all platforms).
(setq frame-resize-pixelwise t)

;; Disable UI elements
(use-package menu-bar
  :config (menu-bar-mode 0))

(use-package tool-bar
  :config
  (scroll-bar-mode 0)
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
  :bind (("C-c =" . text-scale-increase)
         ("C-c -" . text-scale-decrease)))

;; Powerline
(use-package powerline

  :config
  (setq-default powerline-default-separator 'wave))

;; Subtle dim-on-blur: a frame fades slightly when it loses focus, so it's
;; obvious at a glance which window is active without needing system-wide
;; window borders. NS-frame focus (after-focus-change-function) is unrelated to
;; the tmux focus-events that leak over ssh, so there is no conflict there.
(defcustom mgrbyte-frame-unfocused-alpha 90
  "Opacity (0-100) applied to a graphical frame when it loses focus."
  :type 'integer
  :group 'frames)

(defun mgrbyte-dim-frame-on-focus-change ()
  "Restore opacity to focused frames and dim the rest."
  (dolist (frame (frame-list))
    (when (display-graphic-p frame)
      (pcase (frame-focus-state frame)
        ('t (set-frame-parameter frame 'alpha 100))
        ('nil (set-frame-parameter frame 'alpha mgrbyte-frame-unfocused-alpha))))))

(add-function :after after-focus-change-function
              #'mgrbyte-dim-frame-on-focus-change)

;; Late initialization - runs after init.el fully loaded (daemon startup)
(add-hook 'emacs-startup-hook
          (lambda ()
            (add-to-list 'default-frame-alist (cons 'font mgrbyte-default-font))
            (when-let ((tokyo-lib (locate-library "tokyo-night-theme")))
              (add-to-list 'custom-theme-load-path (file-name-directory tokyo-lib)))
            (load-theme 'tokyo-night t)
            ;; Overrides for colour-blind accessibility
            (set-face-foreground 'font-lock-function-name-face "#b4f9f8")
            (set-face-foreground 'font-lock-doc-face "#41a6b5")
            (set-face-foreground 'font-lock-builtin-face "#ff9e64")
            (set-face-foreground 'font-lock-constant-face "#7ddfff")))

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
