;;; init-mail.el --- Mail configuration -*- lexical-binding: t -*-
;;; Commentary:
;; Gnus + IMAP/SMTP mail.
;; Server hosts appear as auth-source lookup keys; ports, credentials,
;; and email addresses resolved from encrypted netrc at runtime.
;;; Code:

(require 'auth-source)

(defun mgrbyte-auth-port (host)
  "Return the port for HOST from auth-source, as a number."
  (when-let ((entry (car (auth-source-search :host host :max 1)))
             (port (plist-get entry :port)))
    (cond ((equal port "imaps") 993)
          ((stringp port) (string-to-number port))
          (t port))))

(use-package message
  :defer t
  :custom
  (message-send-mail-function 'smtpmail-send-it))

(use-package smtpmail
  :defer t
  :config
  (let ((port (or (mgrbyte-auth-port "smtp.gmail.com") 587)))
    (setq smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service port
          smtpmail-stream-type (if (= port 465) 'ssl 'starttls))))

(use-package gnus
  :defer t
  :bind (("C-x g" . gnus-other-frame)
         ("C-c C-x m" . gnus))
  :init
  (let ((imap-port (or (mgrbyte-auth-port "imap.gmail.com") 993)))
    (setq gnus-select-method
          `(nnimap "gmail"
                   (nnimap-address "imap.gmail.com")
                   (nnimap-server-port ,imap-port)
                   (nnimap-stream ,(if (memq imap-port '(993 465)) 'ssl 'starttls)))))
  (setq gnus-agent nil
        gnus-gcc-mark-as-read t
        gnus-save-newsrc-file nil
        gnus-read-newsrc-file nil
        gnus-check-new-newsgroups nil)
  :config
  (setq gnus-message-archive-group "nnimap+gmail:[Gmail]/Sent Mail")
  (setq nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash"
        nnmail-expiry-wait 'immediate)
  (setq gnus-posting-styles
        `((".*"
           (name ,user-full-name)
           (address ,user-mail-address)
           ("X-Message-SMTP-Method"
            ,(format "smtp smtp.gmail.com %d"
                     (or (mgrbyte-auth-port "smtp.gmail.com") 587)))))))

(provide 'init-mail)
;;; init-mail.el ends here
