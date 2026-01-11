;;; init-elpa.el --- Package management -*- lexical-binding: t -*-
;;; Commentary:
;; Setup package.el with MELPA and use-package
;;; Code:

(require 'package)
(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/")
    ("melpa" . "https://melpa.org/packages/")))

(setq package-archive-priorities
  '(("gnu" . 0)
    ("melpa-stable" . 5)
    ("melpa" . 10)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Common libraries
(use-package dash)
(use-package f)
(use-package s)
(use-package async
  :functions async-byte-comp-get-allowed-pkgs)

(provide 'init-elpa)
;;; init-elpa.el ends here
