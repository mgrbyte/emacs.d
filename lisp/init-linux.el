;;; init-linux.el --- Linux-specific configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Linux-specific settings
;;; Code:

;; HHKB cmd key sends Super (Mod4) on Linux. Interpret Super as Meta in Emacs.
;; GNOME Shell intercepts Super independently, so it is unaffected.
(when (eq system-type 'gnu/linux)
  (setq x-meta-keysym 'super)

  ;; GTK3 daemon mode cannot create new top-level frames programmatically.
  ;; Use plain (single-frame) layout for ediff's control panel.
  (with-eval-after-load 'ediff-wind
    (when (daemonp)
      (setq ediff-window-setup-function 'ediff-setup-windows-plain))))

(provide 'init-linux)
;;; init-linux.el ends here
