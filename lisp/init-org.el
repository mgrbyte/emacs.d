;;; init-org.el --- Org mode configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Setup org-mode for task management and notes
;;; Code:

(use-package org

  :after (f s)
  :bind (("C-c j" . org-clock-goto)
         ("C-c C-w" . org-refile)
         ("C-c r" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c L" . org-insert-link-global)
         ("C-c O" . org-open-at-point-global))
  :bind (:map org-mode-map
         ("C-TAB" . org-cycle)
         ("C-c v" . org-show-todo-tree)
         ("C-c C-r" . org-refile)
         ("C-c R" . org-reveal))
  :config
  (defun mgrbyte--org-use-speed-commands-for-headings-and-lists ()
    "Activate speed commands on list items too."
    (or (and (looking-at org-outline-regexp) (looking-back "^\**" 100))
        (save-excursion
          (and (looking-at (org-item-re)) (looking-back "^[ \t]*" 100)))))

  (defun mgrbyte--org-mode-ask-effort ()
    "Ask for an effort estimate when clocking in."
    (require 'org)
    (unless (org-entry-get (point) "Effort")
      (let ((effort
             (completing-read
              "Effort: "
              (org-entry-get-multivalued-property (point) "Effort"))))
        (unless (equal effort "")
          (org-set-property "Effort" effort)))))

  (setq org-log-done 'time)
  (setq org-todo-keywords
        (quote ((sequence
                 "TODO(t)"
                 "NEXT(n)"
                 "STARTED(s)"
                 "|"
                 "DONE(d)")
                (sequence
                 "WAITING(w@/!)"
                 "HOLD(h@/!)"
                 "|"
                 "CANCELLED(c@/!)"
                 "PHONE"
                 "MEETING"))))
  (setq org-default-notes-file "~/org/notes.org")
  (setq org-agenda-files (f-entries "~/org" (apply-partially #'s-ends-with? ".org") t))
  (setq org-directory "~/org")
  (setq org-use-effective-time t)
  (setq org-startup-folded t)
  (setq org-cycle-include-plain-lists 'integrate)

  ;; Speed commands
  (add-to-list 'org-speed-commands '("x" org-todo "DONE"))
  (add-to-list 'org-speed-commands '("y" org-todo-yesterday "DONE"))
  (add-to-list 'org-speed-commands '("!" my/org-clock-in-and-track))
  (add-to-list 'org-speed-commands '("s" call-interactively 'org-schedule))
  (add-to-list 'org-speed-commands '("d" my/org-move-line-to-destination))
  (add-to-list 'org-speed-commands '("i" call-interactively 'org-clock-in))
  (add-to-list 'org-speed-commands '("o" call-interactively 'org-clock-out))
  (add-to-list 'org-speed-commands '("$" call-interactively 'org-archive-subtree))

  (org-clock-persistence-insinuate)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (clojure . t)))

  (eval-after-load 'org-agenda
    '(bind-key "i" 'org-agenda-clock-in org-agenda-mode-map))

  (add-hook 'org-clock-in-prepare-hook 'mgrbyte--org-mode-ask-effort))

(provide 'init-org)
;;; init-org.el ends here
