;;; init-helm.el --- Helm configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup Helm for completion and navigation
;;; Code:

(use-package helm

  :diminish helm-mode
  :bind (("C-c h" . helm-command-prefix)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("M-x" . helm-M-x))
  :init
  (unbind-key "C-x c")
  :config
  (helm-mode 1)
  (setq helm-ff-newfile-prompt-p nil)
  ;; open helm buffer inside current window, not occupy whole other window
  (setq helm-split-window-inside-p t)
  ;; move to end or beginning of source when reaching top or bottom of source.
  (setq helm-move-to-line-cycle-in-source t)
  ;; search for library in `require' and `declare-function' sexp.
  (setq helm-ff-search-library-in-sexp t)
  ;; scroll 8 lines other window using M-<next>/M-<prior>s
  (setq helm-scroll-amount 8)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-ff-fuzzy-matching t)
  (setq helm-ff-file-name-history-use-recentf t)
  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))
  (when (executable-find "ack-grep")
    (setq helm-grep-default-command
          "ack-grep -Hn --no-group --no-color %e %p %f"
          helm-grep-default-recurse-command
          "ack-grep -H --no-group --no-color %e %p %f")))

(use-package helm-projectile
  :after (helm projectile)
  :bind (("C-x f" . projectile-find-file)
         ("C-x p g" . projectile-grep))
  :config
  (helm-projectile-on))

;;; Cherry-picked settings from Thierry Volpiatto's config
;;; https://github.com/thierryvolpiatto/emacs-config/blob/main/init-helm.el

;; Adaptive history - helm learns from your selections
(require 'helm-adaptive)
(setq helm-adaptive-history-file
      (expand-file-name "helm-adaptive-history" user-emacs-directory))
(helm-adaptive-mode 1)

;; Better completion styles for Emacs 27+
(add-hook 'helm-mode-hook
          (lambda ()
            (setq completion-styles
                  (if (assq 'flex completion-styles-alist)
                      '(flex)
                    '(helm-flex)))))

;; Icons in helm-find-files and buffers
(with-eval-after-load 'helm-files
  (helm-ff-icon-mode 1))
(with-eval-after-load 'helm-buffers
  (customize-set-variable 'helm-buffers-show-icons t))

;; Popup tips showing buffer/filename in grep results
(with-eval-after-load 'helm-utils
  (helm-popup-tip-mode 1))

;; Better grep with ripgrep (if available)
(with-eval-after-load 'helm-grep
  (when (executable-find "rg")
    (setq helm-grep-ag-command
          "rg --color=always --smart-case --no-heading --line-number %s -- %s %s")))

(provide 'init-helm)
;;; init-helm.el ends here
