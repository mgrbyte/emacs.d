;;; init-remote-mcp.el --- Remote file MCP tools via TRAMP -*- lexical-binding: t; -*-

;; Remote file read/write and command execution MCP tools for Claude Code.
;; Uses Emacs TRAMP to access files on remote servers, allowing
;; Claude Code to run locally while operating on remote project files.
;; No ediff or blocking approval — writes apply directly.

;;; Code:

(require 'tramp)

;;; Helpers

(defun mgrbyte-remote-mcp--tramp-path (host path)
  "Construct a TRAMP path from HOST and PATH."
  (format "/rpc:%s:%s" host path))

;;; Handlers

(defun mgrbyte-remote-mcp-ide--read-file (host path)
  "Read a file from HOST at PATH via TRAMP."
  (with-temp-buffer
    (insert-file-contents (mgrbyte-remote-mcp--tramp-path host path))
    (buffer-string)))

(defun mgrbyte-remote-mcp-ide--write-file (host path content)
  "Write CONTENT to PATH on HOST via TRAMP. Flushes TRAMP cache after write."
  (let ((tramp-path (mgrbyte-remote-mcp--tramp-path host path)))
    (with-temp-buffer
      (insert content)
      (write-region (point-min) (point-max) tramp-path))
    (tramp-flush-directory-properties
     (tramp-dissect-file-name tramp-path) (file-name-directory tramp-path))
    (format "Written %d bytes to %s" (length content) path)))

(defun mgrbyte-remote-mcp-ide--exec (host command &optional working-dir)
  "Execute COMMAND on HOST inside nix-user-chroot.
Optional WORKING-DIR sets the remote working directory."
  (let* ((chroot-cmd (if working-dir
                        (format "cd %s && %s"
                                (shell-quote-argument working-dir)
                                command)
                      command))
         (remote-cmd (format "~/.local/bin/nix-user-chroot ~/.nix zsh -lc %s"
                             (shell-quote-argument chroot-cmd))))
    (with-temp-buffer
      (let ((exit-code (call-process "ssh" nil t nil host remote-cmd)))
        (format "Exit code: %d\n%s" exit-code (buffer-string))))))

;;; Tool registration

(defun mgrbyte-remote-mcp-ide--register-tools ()
  "Register remote MCP tools on the claude-code-ide HTTP server."
  (let ((host-arg '(:name "host" :type string :required t
                    :description "Remote hostname (e.g., dl6)"))
        (path-arg '(:name "path" :type string :required t
                    :description "Absolute path on the remote server")))
    (claude-code-ide-make-tool
     :name "remoteReadFile"
     :function #'mgrbyte-remote-mcp-ide--read-file
     :description "Read a file from a remote server via TRAMP."
     :args (list host-arg path-arg))
    (claude-code-ide-make-tool
     :name "remoteWriteFile"
     :function #'mgrbyte-remote-mcp-ide--write-file
     :description "Write content to a file on a remote server. Applies directly, no approval needed."
     :args (list host-arg path-arg
                 '(:name "content" :type string :required t
                   :description "File content to write")))
    (claude-code-ide-make-tool
     :name "remoteExec"
     :function #'mgrbyte-remote-mcp-ide--exec
     :description "Execute a command on a remote server inside nix-user-chroot."
     :args (list host-arg
                 '(:name "command" :type string :required t
                   :description "Shell command to execute")
                 '(:name "working_dir" :type string :optional t
                   :description "Working directory on the remote server")))))

(provide 'init-remote-mcp)
;;; init-remote-mcp.el ends here
