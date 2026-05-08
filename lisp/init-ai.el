;;; init-ai.el --- AI/LLM integration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for AI assistants: gptel (local LLMs), MCP, Claude Code
;; Packages are installed via Nix (see nix-config/home/packages.nix)
;;; Code:

;; gptel - LLM client (for local models via Ollama, not Claude)
(use-package gptel
  :bind (("C-c g g" . gptel)
         ("C-c g s" . gptel-send)
         ("C-c g m" . gptel-menu)
         ("C-c g a" . gptel-add)
         ("C-c g f" . gptel-add-file))
  :config
  ;; Use Ollama for local models (no API key needed)
  (setq gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '(llama3.2 mistral codellama)))
  (setq gptel-model 'llama3.2)
  (setq gptel-default-mode 'markdown-mode))

;; mcp.el - Model Context Protocol client
(use-package mcp
  :commands (mcp-hub mcp-hub-start-all-server)
  :config
  (setq mcp-hub-servers
        '(("serena" . (:command ("uvx" "serena")
                       :args ("--config" "~/.config/serena/settings.yml"))))))

;; gptel-mcp integration
(with-eval-after-load 'gptel
  (with-eval-after-load 'mcp
    (require 'gptel-mcp nil t)))

;; org-mcp - MCP server for Org-mode
(use-package org-mcp
  :after org
  :config
  (setq org-mcp-allowed-files org-agenda-files))

;; claude-code-ide.el — loaded for MCP server and IDE tools only.
;; Terminal rendering is handled by alacritty/tmux via mgrbyte-claude-ide-external.
(use-package claude-code-ide
  :demand t
  :config
  (setq claude-code-ide-use-side-window nil)
  (claude-code-ide-emacs-tools-setup))

;; Fix: revert file buffer after accepting ediff changes from claude-code-ide.
;; Ediff leaves buffer A dirty after comparison. Without this, subsequent edits
;; to the same file cause A/B buffer swap and "changed on disk" prompts.
(defun mgrbyte-revert-buffer-after-ediff-accept (tab-name &optional session)
  "Revert file buffer after accepting ediff changes from claude-code-ide."
  (let* ((active-diffs (claude-code-ide-mcp--get-active-diffs session))
         (diff-info (gethash tab-name active-diffs)))
    (when diff-info
      (let ((file-buf (alist-get 'buffer-A diff-info)))
        (run-with-idle-timer 1.0 nil
                             (lambda ()
                               (when (and file-buf
                                          (buffer-live-p file-buf)
                                          (buffer-file-name file-buf))
                                 (with-current-buffer file-buf
                                   (revert-buffer t t t)))))))))

(advice-add 'claude-code-ide-mcp--handle-ediff-quit
            :after #'mgrbyte-revert-buffer-after-ediff-accept)

;; emacs-mcp-server — exposes Emacs to Claude Code via MCP unix socket
(use-package mcp-server
  :demand t
  :config
  (mcp-server-start-unix)
  :custom
  (mcp-server-security-dangerous-functions '(find-file with-current-buffer insert-file-contents))
  (mcp-server-security-allowed-dangerous-functions '(find-file with-current-buffer)))

;; Remote file and command MCP tools via TRAMP
(require 'init-remote-mcp)

;;; Remote MCP tools — registered on HTTP server (emacs-tools)

(defun mgrbyte-remote-mcp-ide--read-file (host path)
  "Wrapper: read remote file. HOST and PATH are positional args."
  (mgrbyte-remote-mcp--read-file `((host . ,host) (path . ,path))))

(defun mgrbyte-remote-mcp-ide--grep (host pattern path &optional glob)
  "Wrapper: grep remote files. HOST, PATTERN, PATH, GLOB are positional."
  (mgrbyte-remote-mcp--grep `((host . ,host) (pattern . ,pattern)
                              (path . ,path) (glob . ,glob))))

(defun mgrbyte-remote-mcp-ide--list-files (host path &optional pattern)
  "Wrapper: list remote files. HOST, PATH, PATTERN are positional."
  (mgrbyte-remote-mcp--list-files `((host . ,host) (path . ,path)
                                    (pattern . ,pattern))))

(defun mgrbyte-remote-mcp-ide--exec (host command &optional working-dir)
  "Wrapper: execute remote command. HOST, COMMAND, WORKING-DIR are positional."
  (mgrbyte-remote-mcp--exec `((host . ,host) (command . ,command)
                              (working_dir . ,working-dir))))

(defun mgrbyte-remote-mcp-ide--ediff-confirm (tramp-path old-content new-content)
  "Show ediff of OLD-CONTENT vs NEW-CONTENT for TRAMP-PATH.
Blocks via `recursive-edit' until user quits ediff.
Returns non-nil if user accepted the edit."
  (let ((result nil)
        (buf-a (generate-new-buffer
                (format "*remote-old:%s*" (file-name-nondirectory tramp-path))))
        (buf-b (generate-new-buffer
                (format "*remote-new:%s*" (file-name-nondirectory tramp-path))))
        (saved-winconf (current-window-configuration)))
    (with-current-buffer buf-a
      (insert old-content)
      (let ((mode (assoc-default tramp-path auto-mode-alist 'string-match)))
        (when mode (ignore-errors (funcall mode)))))
    (with-current-buffer buf-b
      (insert new-content)
      (let ((mode (assoc-default tramp-path auto-mode-alist 'string-match)))
        (when mode (ignore-errors (funcall mode)))))
    (let ((ediff-quit-hook
           (list (lambda ()
                   (setq result (y-or-n-p "Accept remote edit? "))
                   (exit-recursive-edit)))))
      (switch-to-buffer buf-a)
      (dolist (window (window-list))
        (when (window-parameter window 'window-side)
          (delete-window window)))
      (delete-other-windows)
      (let ((ediff-window-setup-function 'ediff-setup-windows-plain)
            (ediff-split-window-function 'split-window-horizontally))
        (ediff-buffers buf-a buf-b))
      (recursive-edit))
    (when (buffer-live-p buf-a) (kill-buffer buf-a))
    (when (buffer-live-p buf-b) (kill-buffer buf-b))
    (condition-case nil
        (set-window-configuration saved-winconf)
      (error nil))
    result))

(defun mgrbyte-remote-mcp-ide--edit-file (host path old-string new-string)
  "Edit a remote file with ediff confirmation. Blocks until user accepts or rejects."
  (let* ((tramp-path (format "/rpc:%s:%s" host path))
         (old-content (with-temp-buffer
                        (insert-file-contents tramp-path)
                        (buffer-string))))
    (unless (string-match-p (regexp-quote old-string) old-content)
      (error "old_string not found in %s" path))
    (let ((new-content (replace-regexp-in-string
                        (regexp-quote old-string) new-string old-content t t)))
      (if (mgrbyte-remote-mcp-ide--ediff-confirm tramp-path old-content new-content)
          (progn
            (with-temp-buffer
              (insert new-content)
              (write-region (point-min) (point-max) tramp-path))
            (format "Edited %s" path))
        (format "Edit rejected by user for %s" path)))))

(defun mgrbyte-remote-mcp-ide--write-file (host path content)
  "Write a remote file, showing ediff for existing files. Blocks until user accepts."
  (let ((tramp-path (format "/rpc:%s:%s" host path)))
    (if (file-exists-p tramp-path)
        (let ((old-content (with-temp-buffer
                             (insert-file-contents tramp-path)
                             (buffer-string))))
          (if (mgrbyte-remote-mcp-ide--ediff-confirm tramp-path old-content content)
              (progn
                (with-temp-buffer
                  (insert content)
                  (write-region (point-min) (point-max) tramp-path))
                (format "Written %d bytes to %s" (length content) path))
            (format "Write rejected by user for %s" path)))
      (with-temp-buffer
        (insert content)
        (write-region (point-min) (point-max) tramp-path))
      (format "Written %d bytes to %s (new file)" (length content) path))))

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
     :name "remoteGrep"
     :function #'mgrbyte-remote-mcp-ide--grep
     :description "Search for a pattern in files on a remote server. Returns matching lines."
     :args (list host-arg
                 '(:name "pattern" :type string :required t
                   :description "Grep pattern (regex)")
                 path-arg
                 '(:name "glob" :type string :optional t
                   :description "File glob pattern (e.g., *.py)")))
    (claude-code-ide-make-tool
     :name "remoteListFiles"
     :function #'mgrbyte-remote-mcp-ide--list-files
     :description "List files on a remote server matching a glob pattern."
     :args (list host-arg path-arg
                 '(:name "pattern" :type string :optional t
                   :description "File name pattern (e.g., *.py)")))
    (claude-code-ide-make-tool
     :name "remoteExec"
     :function #'mgrbyte-remote-mcp-ide--exec
     :description "Execute a command on a remote server inside nix-user-chroot."
     :args (list host-arg
                 '(:name "command" :type string :required t
                   :description "Shell command to execute")
                 '(:name "working_dir" :type string :optional t
                   :description "Working directory on the remote server")))
    (claude-code-ide-make-tool
     :name "remoteEditFile"
     :function #'mgrbyte-remote-mcp-ide--edit-file
     :description "Edit a file on a remote server by string replacement."
     :args (list host-arg path-arg
                 '(:name "old_string" :type string :required t
                   :description "Exact string to find and replace")
                 '(:name "new_string" :type string :required t
                   :description "Replacement string")))
    (claude-code-ide-make-tool
     :name "remoteWriteFile"
     :function #'mgrbyte-remote-mcp-ide--write-file
     :description "Write content to a file on a remote server."
     :args (list host-arg path-arg
                 '(:name "content" :type string :required t
                   :description "File content to write")))))

(mgrbyte-remote-mcp-ide--register-tools)

;; mgrbyte-claude — external terminal integration and auto-dependency discovery
(require 'mgrbyte-claude)
(mgrbyte-claude-mode 1)

(provide 'init-ai)
;;; init-ai.el ends here
