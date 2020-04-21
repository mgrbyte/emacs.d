;;; mgrbyte.el --- Custom modes -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Matthew Russell

;; Author: Matthew Russell <matthew.russell@horizon5.org>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;
;;; Code:

(defgroup mgrbyte nil
  "Mgrbyte Development environment"
  :group 'tools
  :prefix "mgrbyte:")

(defgroup mgrbyte-modes nil
  "Modes provided by Mgrbyte"
  :group 'mgrbyte)

(defgroup mgrbyte-keys nil
  "Mgrbyte keybindings."
  :group 'mgrbyte-modes
  :prefix "mgrbyte:key-")

(defvar mgrbyte-keymap
  (let ((map (make-sparse-keymap)))
    ;; Keys for custom mgrbyte defuns
    (define-key map (kbd "C-c n d") 'mgrbyte-insert-current-date)
    (define-key map (kbd "C-c n t") 'mgrbyte-insert-current-time)
    (define-key map (kbd "C-c n c") 'mgrbyte-filename-to-clipboard)

    ;; Overrides for builtin commands
    (define-key map (kbd "M-c") 'capitalize-word)
    (define-key map (kbd "M-g f") 'list-faces-display)
    (define-key map (kbd "M-s x") 'replace-regexp)
    (define-key map (kbd "M-z") 'goto-char)
    (define-key map (kbd "C-x w") 'woman)

    map)
  "Keymap for `mgrbyte-keys'.")

;;;###autoload
(define-minor-mode mgrbyte-mode
  "Toggle Mgrbyte mode.

\\{mgrbyte-keymap}
Interactively with no argument, this command toggles the mode.
When activated, this mode makes the 'standard' mgrbyte keybindings
take effect."
  nil
  :lighter " Mgrbyte"
  :global t
  :keymap mgrbyte-keymap
  :group 'mgrbyte-keys)

(defun mgrbyte-log-if-loaded (pkgname)
  "Print a message after some package has been loaded.
Argument PKGNAME The name of the package being loaded."
  (eval-after-load pkgname
    (message (format "Loaded %s" pkgname))))

(defun py-insert-debug ()
  "Insert python debug commands.

Quick-Insert python debug mode."
  (interactive)
  (declare-function python-nav-end-of-statement python nil)
  (let ((pdb-text "import pdb; pdb.set_trace()"))
    (python-nav-end-of-statement)
    (newline-and-indent)
    (insert pdb-text)))

;; Confirm switch to non-existent buffer
(defadvice switch-to-buffer (around confirm-non-existing-buffers activate)
  "Switch to non-existing buffers only upon confirmation."
  (interactive "BSwitch to buffer: ")
  (if (or (get-buffer (ad-get-arg 0))
          (y-or-n-p (format "´%s' does not exist, create? "(ad-get-arg 0))))
      ad-do-it))

(defun mgrbyte-filename-to-clipboard ()
  "Put the absolute path to the current file name on the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun mgrbyte-buffer-name-to-clipboard ()
  "Put the current buffer name on the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defvar current-date-format "%Y-%m-%d"
  "Format of date to insert with `insert-current-date'.
See documentation of `format-time-string' for possible replacements")


(defvar current-time-format "%a %H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")


(defun mgrbyte-insert-current-date ()
  "Insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-format (current-time))))

(defun mgrbyte-insert-current-time ()
  "Insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time))))

(defun mgrbyte-sort-directories-first ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defun mgrbyte-insert-package-summary ()
  "Insert the summary of an ELPA package at point."
  (interactive)
  (let ((pkg-name
         (if (region-active-p)
             (buffer-substring (region-beginning) (region-end))
            (read-string "Package: "))))
    (message (format "Looking up summary for package %s" pkg-name))
    (let* ((pkg (intern pkg-name))
           (pkg-vec (assq pkg package-alist))
           (pkg-desc-v (cdr pkg-vec))
           (pkg-desc-vlen (length pkg-desc-v)))
      (if (eq pkg-desc-v nil)
          (message "Could not find package info"))
      (when pkg-desc-vlen
        (let ((summary (elt pkg-desc-v (- pkg-desc-vlen 1))))
          (insert summary))))))

(defun mgrbyte-sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file.
Nicked from http://emacsredux.com/blog/2013/04/21/edit-files-as-root/"
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun mgrbyte-delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file except the very last."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines))))


(provide 'mgrbyte)
;;; mgrbyte.el ends here
