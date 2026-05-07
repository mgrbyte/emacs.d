;;; init-remote-mcp.el --- Remote file MCP tools via TRAMP -*- lexical-binding: t; -*-

;; Remote file and command execution MCP tools for Claude Code.
;; Uses Emacs TRAMP to access files on remote servers, allowing
;; Claude Code to run locally with full IDE integration while
;; operating on remote project files.

;;; Code:

(require 'mcp-server-tools)
(require 'tramp)

;;; Helpers

(defun mgrbyte-remote-mcp--tramp-path (args)
  "Construct a TRAMP path from host and path in ARGS."
  (format "/rpc:%s:%s" (alist-get 'host args) (alist-get 'path args)))

(defun mgrbyte-remote-mcp--remote-shell (args cmd)
  "Run shell CMD on the remote host from ARGS, using path as working directory.
Returns the command output as a string."
  (let ((default-directory (mgrbyte-remote-mcp--tramp-path args)))
    (with-temp-buffer
      (let ((exit-code (process-file-shell-command cmd nil t)))
        (list exit-code (buffer-string))))))

(defconst mgrbyte-remote-mcp--host-schema
  '(host . ((type . "string") (description . "Remote hostname (e.g., dl6)")))
  "Shared JSON schema for the host parameter.")

(defconst mgrbyte-remote-mcp--path-schema
  '(path . ((type . "string") (description . "Absolute path on the remote server")))
  "Shared JSON schema for the path parameter.")

(defun mgrbyte-remote-mcp--schema (properties required)
  "Build a JSON schema object with PROPERTIES alist and REQUIRED vector."
  `((type . "object")
    (properties . ,properties)
    (required . ,required)))

;;; Handlers

(defun mgrbyte-remote-mcp--read-file (args)
  "Read a file from a remote server. ARGS contains host and path."
  (with-temp-buffer
    (insert-file-contents (mgrbyte-remote-mcp--tramp-path args))
    (buffer-string)))

(defun mgrbyte-remote-mcp--write-file (args)
  "Write content to a file on a remote server. ARGS contains host, path, content."
  (let ((content (alist-get 'content args))
        (tramp-path (mgrbyte-remote-mcp--tramp-path args)))
    (with-temp-buffer
      (insert content)
      (write-region (point-min) (point-max) tramp-path))
    (format "Written %d bytes to %s" (length content) tramp-path)))

(defun mgrbyte-remote-mcp--edit-file (args)
  "Edit a file on a remote server by replacing a string. ARGS contains host, path, old_string, new_string."
  (let ((old-string (alist-get 'old_string args))
        (new-string (alist-get 'new_string args))
        (tramp-path (mgrbyte-remote-mcp--tramp-path args)))
    (with-temp-buffer
      (insert-file-contents tramp-path)
      (goto-char (point-min))
      (unless (search-forward old-string nil t)
        (error "old_string not found in %s" tramp-path))
      (replace-match new-string t t)
      (write-region (point-min) (point-max) tramp-path))
    (format "Edited %s" tramp-path)))

(defun mgrbyte-remote-mcp--grep (args)
  "Search for a pattern in files on a remote server. ARGS contains host, pattern, path, and optional glob."
  (let* ((pattern (alist-get 'pattern args))
         (glob (alist-get 'glob args))
         (cmd (format "grep -rn %s %s ."
                      (if glob (format "--include=%s" (shell-quote-argument glob)) "")
                      (shell-quote-argument pattern))))
    (pcase-let ((`(,exit-code ,output) (mgrbyte-remote-mcp--remote-shell args cmd)))
      (cond
       ((= exit-code 0) output)
       ((= exit-code 1) "No matches found.")
       (t (format "grep failed (exit %d): %s" exit-code output))))))

(defun mgrbyte-remote-mcp--list-files (args)
  "List files on a remote server. ARGS contains host, path, and optional pattern."
  (let* ((pattern (alist-get 'pattern args))
         (cmd (if pattern
                  (format "find . -name %s -type f | head -500" (shell-quote-argument pattern))
                "find . -type f | head -500")))
    (cadr (mgrbyte-remote-mcp--remote-shell args cmd))))

(defun mgrbyte-remote-mcp--exec (args)
  "Execute a command on a remote server inside nix-user-chroot. ARGS contains host, command, and optional working_dir."
  (let* ((host (alist-get 'host args))
         (command (alist-get 'command args))
         (working-dir (alist-get 'working_dir args))
         (chroot-cmd (if working-dir
                        (format "cd %s && %s"
                                (shell-quote-argument working-dir)
                                command)
                      command))
         (ssh-cmd (format "ssh %s '~/.local/bin/nix-user-chroot ~/.nix zsh -lc %s'"
                          (shell-quote-argument host)
                          (shell-quote-argument chroot-cmd))))
    (with-temp-buffer
      (let ((exit-code (call-process-shell-command ssh-cmd nil t)))
        (format "Exit code: %d\n%s" exit-code (buffer-string))))))

;;; Tool registration

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "remote-read-file"
  :title "Remote Read File"
  :description "Read a file from a remote server via TRAMP. Returns the file content."
  :input-schema (mgrbyte-remote-mcp--schema
                 `(,mgrbyte-remote-mcp--host-schema ,mgrbyte-remote-mcp--path-schema)
                 ["host" "path"])
  :function #'mgrbyte-remote-mcp--read-file))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "remote-write-file"
  :title "Remote Write File"
  :description "Write content to a file on a remote server via TRAMP."
  :input-schema (mgrbyte-remote-mcp--schema
                 `(,mgrbyte-remote-mcp--host-schema
                   ,mgrbyte-remote-mcp--path-schema
                   (content . ((type . "string")
                               (description . "File content to write"))))
                 ["host" "path" "content"])
  :function #'mgrbyte-remote-mcp--write-file))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "remote-edit-file"
  :title "Remote Edit File"
  :description "Edit a file on a remote server by replacing a string. The old_string must match exactly."
  :input-schema (mgrbyte-remote-mcp--schema
                 `(,mgrbyte-remote-mcp--host-schema
                   ,mgrbyte-remote-mcp--path-schema
                   (old_string . ((type . "string")
                                  (description . "Exact string to find and replace")))
                   (new_string . ((type . "string")
                                  (description . "Replacement string"))))
                 ["host" "path" "old_string" "new_string"])
  :function #'mgrbyte-remote-mcp--edit-file))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "remote-grep"
  :title "Remote Grep"
  :description "Search for a pattern in files on a remote server. Returns matching lines with file paths and line numbers."
  :input-schema (mgrbyte-remote-mcp--schema
                 `(,mgrbyte-remote-mcp--host-schema
                   (pattern . ((type . "string")
                               (description . "Grep pattern (regex)")))
                   ,mgrbyte-remote-mcp--path-schema
                   (glob . ((type . "string")
                            (description . "File glob pattern (e.g., *.py). Optional."))))
                 ["host" "pattern" "path"])
  :function #'mgrbyte-remote-mcp--grep))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "remote-list-files"
  :title "Remote List Files"
  :description "List files on a remote server matching a glob pattern."
  :input-schema (mgrbyte-remote-mcp--schema
                 `(,mgrbyte-remote-mcp--host-schema
                   ,mgrbyte-remote-mcp--path-schema
                   (pattern . ((type . "string")
                               (description . "File name pattern (e.g., *.py). Optional."))))
                 ["host" "path"])
  :function #'mgrbyte-remote-mcp--list-files))

(mcp-server-register-tool
 (make-mcp-server-tool
  :name "remote-exec"
  :title "Remote Execute Command"
  :description "Execute a command on a remote server inside the nix-user-chroot environment. Use for tests, docker, linting."
  :input-schema (mgrbyte-remote-mcp--schema
                 `(,mgrbyte-remote-mcp--host-schema
                   (command . ((type . "string")
                               (description . "Shell command to execute")))
                   (working_dir . ((type . "string")
                                   (description . "Working directory on the remote server. Optional."))))
                 ["host" "command"])
  :function #'mgrbyte-remote-mcp--exec))

(provide 'init-remote-mcp)
;;; init-remote-mcp.el ends here
