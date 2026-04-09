;;; init-linux.el --- Linux-specific configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Linux-specific settings
;;; Code:

(when (eq system-type 'gnu/linux)
  ;; HHKB cmd key sends Super (Mod4) on Linux. Treat Super as Meta in Emacs.
  ;; GNOME Shell intercepts Super independently so it is unaffected.
  (setq x-super-keysym 'meta)
  ;; Allow pixel-level frame resizing so tiling (TileDevTools) fills the full
  ;; work area without a gap from character-row boundary snapping.
  (setq frame-resize-pixelwise t))

(provide 'init-linux)
;;; init-linux.el ends here
