;;; init-linux.el --- Linux-specific configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Linux-specific settings
;;; Code:

;; HHKB cmd key sends Super (Mod4) on Linux. Interpret Super as Meta in Emacs.
;; GNOME Shell intercepts Super independently, so it is unaffected.
(when (eq system-type 'gnu/linux)
  (setq x-meta-keysym 'super)

  ;; Emacs 30 daemon mode: make-frame no longer reliably inherits the terminal
  ;; from the current frame. Passing 'display causes GTK to open a new display
  ;; connection which crashes fatally on Wayland. Pass the terminal object
  ;; directly so make-frame reuses the existing connection.
  (with-eval-after-load 'ediff-wind
    (advice-add 'ediff-setup-control-frame :around
                (lambda (orig-fun &rest args)
                  (let ((ediff-control-frame-parameters
                         (cons (cons 'terminal (frame-terminal (selected-frame)))
                               ediff-control-frame-parameters)))
                    (apply orig-fun args))))))

(provide 'init-linux)
;;; init-linux.el ends here
