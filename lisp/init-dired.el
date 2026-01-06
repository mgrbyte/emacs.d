;;; init-dired.el --- Dired configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup dired and dirvish
;;; Code:

(use-package dired
  :after mgrbyte
  :config
  (advice-add 'dired-readin :after #'mgrbyte-sort-directories-first))

(use-package dired-x
  :config
  (setq-default dired-omit-files-p nil)
  (setq dired-omit-files
        (concat dired-omit-files "\\|^\\..+$")))

(use-package dirvish
  :ensure t
  :after dired
  :init
  ;; Disable dirvish mode-line (use standard dired mode-line)
  (setq dirvish-use-mode-line nil)
  ;; Keep header with path breadcrumbs
  (setq dirvish-header-line-format '(:left (path) :right ()))
  ;; File attributes to show
  (setq dirvish-attributes '(file-size file-time))
  :config
  (dirvish-override-dired-mode))

(use-package ls-lisp
  :config (setq ls-lisp-use-insert-directory-program nil))

(use-package recentf
  :bind (("C-x r e" . recentf-edit-list))
  :config
  (add-to-list 'recentf-exclude (expand-file-name "~/git/emacs.d/.cask.*")))

(provide 'init-dired)
;;; init-dired.el ends here
