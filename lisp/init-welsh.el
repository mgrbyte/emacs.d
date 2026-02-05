;;; init-welsh.el --- Welsh character input -*- lexical-binding: t -*-
;;; Commentary:
;; Keybindings for Welsh accented characters (to-bach/circumflex).
;; Uses Super modifier (Right Command on macOS).
;;; Code:

(global-set-key (kbd "s-a") (lambda () (interactive) (insert "â")))
(global-set-key (kbd "s-A") (lambda () (interactive) (insert "Â")))
(global-set-key (kbd "s-e") (lambda () (interactive) (insert "ê")))
(global-set-key (kbd "s-E") (lambda () (interactive) (insert "Ê")))
(global-set-key (kbd "s-i") (lambda () (interactive) (insert "î")))
(global-set-key (kbd "s-I") (lambda () (interactive) (insert "Î")))
(global-set-key (kbd "s-o") (lambda () (interactive) (insert "ô")))
(global-set-key (kbd "s-O") (lambda () (interactive) (insert "Ô")))
(global-set-key (kbd "s-u") (lambda () (interactive) (insert "û")))
(global-set-key (kbd "s-U") (lambda () (interactive) (insert "Û")))
(global-set-key (kbd "s-w") (lambda () (interactive) (insert "ŵ")))
(global-set-key (kbd "s-W") (lambda () (interactive) (insert "Ŵ")))
(global-set-key (kbd "s-y") (lambda () (interactive) (insert "ŷ")))
(global-set-key (kbd "s-Y") (lambda () (interactive) (insert "Ŷ")))

(provide 'init-welsh)
;;; init-welsh.el ends here
