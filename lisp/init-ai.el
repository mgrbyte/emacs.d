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

;; Cleanup: run --cleanup-diff after ediff quit so buffer-B and diff metadata
;; are freed immediately rather than waiting for a close_tab call.
(defun mgrbyte-cleanup-diff-after-ediff-quit (tab-name &optional session)
  "Clean up diff buffers for TAB-NAME after ediff quit."
  (run-with-idle-timer
   2.0 nil
   (lambda ()
     (when (fboundp 'claude-code-ide-mcp--cleanup-diff)
       (condition-case nil
           (claude-code-ide-mcp--cleanup-diff tab-name session)
         (error nil))))))

(advice-add 'claude-code-ide-mcp--handle-ediff-quit
            :after #'mgrbyte-cleanup-diff-after-ediff-quit)

;; Manual cleanup for orphaned ediff buffers
(defun mgrbyte-ediff-cleanup-stale-buffers ()
  "Kill all orphaned ediff buffers (control panels, diff output, etc.)."
  (interactive)
  (let ((killed 0))
    (dolist (buf (buffer-list))
      (let ((name (buffer-name buf)))
        (when (and name
                   (or (string-match-p "\\*[Ee]diff" name)
                       (string-match-p "\\*ediff-diff" name)
                       (string-match-p "\\*ediff-fine-diff" name)
                       (string-match-p "\\*ediff-errors" name)
                       (string-match-p "\\*ediff-custom-diff" name)))
          (kill-buffer buf)
          (setq killed (1+ killed)))))
    (message "Killed %d stale ediff buffer(s)" killed)))

;; emacs-mcp-server — exposes Emacs to Claude Code via MCP unix socket
(use-package mcp-server
  :demand t
  :config
  (mcp-server-start-unix)
  :custom
  (mcp-server-security-dangerous-functions '(find-file with-current-buffer insert-file-contents))
  (mcp-server-security-allowed-dangerous-functions '(find-file with-current-buffer)))

;; mgrbyte-claude — external terminal integration and auto-dependency discovery
(require 'mgrbyte-claude)
(mgrbyte-claude-mode 1)

(provide 'init-ai)
;;; init-ai.el ends here
