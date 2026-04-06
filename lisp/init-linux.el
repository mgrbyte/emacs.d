;;; init-linux.el --- Linux-specific configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Linux-specific settings
;;; Code:

;; HHKB cmd key sends Super (Mod4) on Linux. Interpret Super as Meta in Emacs.
;; GNOME Shell intercepts Super independently, so it is unaffected.
(when (eq system-type 'gnu/linux)
  (setq x-meta-keysym 'super)

  ;; Emacs 30 daemon mode: make-frame no longer reliably inherits the display
  ;; from the current frame, falling back to the daemon's TTY and crashing.
  ;; Explicitly pass the current frame's display to ediff's control frame.
  (with-eval-after-load 'ediff-wind
    (advice-add 'ediff-setup-control-frame :around
                (lambda (orig-fun &rest args)
                  (let ((ediff-control-frame-parameters
                         (cons (cons 'display (frame-parameter (selected-frame) 'display))
                               ediff-control-frame-parameters)))
                    (apply orig-fun args))))))

(provide 'init-linux)
;;; init-linux.el ends here
