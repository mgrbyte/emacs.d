;;; init-clojure.el --- Clojure configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup Clojure mode
;;; Code:

(use-package clojure-mode
  :ensure t
  :preface
  (defun mgrbyte-setup-clj ()
    (yas-minor-mode 1))
  :hook (clojure-mode . mgrbyte-setup-clj)
  :config
  (require 'flycheck-clj-kondo)
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

(use-package flycheck-clj-kondo
  :ensure t)

(provide 'init-clojure)
;;; init-clojure.el ends here
