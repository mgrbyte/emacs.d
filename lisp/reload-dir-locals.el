;;; reload-dir-locals.el --- Reload dir-locals -*- lexical-binding: t -*-
;;; Commentary:
;; Utility to reload .dir-locals.el for the current buffer
;;; Code:

(defun reload-dir-locals-for-current-buffer ()
  "Reload dir-locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun reload-dir-locals-for-all-buffers-in-directory ()
  "Reload dir-locals for all buffers in the current directory."
  (interactive)
  (let ((dir (file-name-directory (or buffer-file-name default-directory))))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and buffer-file-name
                   (string-prefix-p dir buffer-file-name))
          (hack-dir-local-variables-non-file-buffer))))))

(provide 'reload-dir-locals)
;;; reload-dir-locals.el ends here
