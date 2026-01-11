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

;; Multi-monitor support: prefer external monitor over built-in
(defun mgrbyte-get-external-monitor-workarea ()
  "Get the workarea of external monitor (non-primary or largest)."
  (let* ((monitors (display-monitor-attributes-list))
         ;; External monitor is usually not at origin (0,0) or is larger
         (external (cl-find-if
                    (lambda (m)
                      (let ((geom (cdr (assq 'geometry m))))
                        (> (nth 0 geom) 0)))  ; x position > 0 means not primary
                    monitors))
         ;; Fallback: pick largest by pixel area if all at origin
         (largest (car (cl-sort (copy-sequence monitors)
                                (lambda (a b)
                                  (let ((ga (cdr (assq 'geometry a)))
                                        (gb (cdr (assq 'geometry b))))
                                    (> (* (nth 2 ga) (nth 3 ga))
                                       (* (nth 2 gb) (nth 3 gb)))))))))
    (cdr (assq 'workarea (or external largest)))))

(defun mgrbyte-frame-to-external-maximized ()
  "Move frame to external monitor and maximize it."
  (interactive)
  (when-let ((workarea (mgrbyte-get-external-monitor-workarea)))
    ;; Clear any existing fullscreen state first
    (set-frame-parameter nil 'fullscreen nil)
    ;; Move to external monitor
    (set-frame-position (selected-frame) (nth 0 workarea) (nth 1 workarea))
    ;; Small delay for macOS to process the move, then maximize
    (run-at-time 0.1 nil (lambda () (set-frame-parameter nil 'fullscreen 'maximized)))))

;; Maximize frames by default (works better than hooks for multi-monitor)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Late initialization - runs after init.el fully loaded (daemon startup)
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (window-system)
              (set-face-attribute 'default nil :font
                                  (if (eq system-type 'darwin) "Menlo 14" "Ubuntu Mono 14")))
            (load-theme 'abyss t)))

;; New client frames - runs for each emacsclient -c
(add-hook 'server-after-make-frame-hook
          (lambda ()
            (when (display-graphic-p)
              (run-at-time 0.3 nil #'mgrbyte-frame-to-external-maximized))))

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
