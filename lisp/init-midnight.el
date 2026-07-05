;;; init-midnight.el --- Reap stale buffers nightly -*- lexical-binding: t -*-
;;; Commentary:
;; Enable `midnight-mode' so `clean-buffer-list' runs once a night and kills
;; buffers left idle past a threshold, keeping a long-running daemon from
;; accumulating hundreds of buffers.
;;
;; The stock `clean-buffer-list-kill-regexps' only matches *Man* buffers, so
;; magit's per-repo buffers (magit:, magit-process:, magit-diff: …) and the
;; common transient popups are added.  The agenda buffer *epicworkmgr* is
;; protected, as are *scratch* and *Messages* (already protected by default).
;;
;; Nightly only — no in-session timer.  Run `M-x clean-buffer-list' for an
;; on-demand sweep.
;;; Code:

(use-package midnight
  :demand t
  :config
  (setq clean-buffer-list-delay-general 3)      ; stale file buffers: 3 days idle
  (setq clean-buffer-list-delay-special 3600)   ; matched/special buffers: 1 hour idle
  (dolist (re '("\\`magit\\(-\\|: \\)"          ; magit:, magit-process:, magit-diff: …
                "\\`\\*helm"
                "\\`\\*\\(Help\\|Compile-Log\\|Warnings\\|Backtrace\\|Completions\\|vc-\\)"))
    (add-to-list 'clean-buffer-list-kill-regexps re))
  (add-to-list 'clean-buffer-list-kill-never-buffer-names "*epicworkmgr*")
  (midnight-mode 1))

(provide 'init-midnight)
;;; init-midnight.el ends here
