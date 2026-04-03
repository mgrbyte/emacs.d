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
  (claude-code-ide-emacs-tools-setup)
  ;; Rebuild tool lists — workaround for load-order bug where
  ;; claude-code-ide-mcp-handlers.el builds the tool list at require
  ;; time (line 64) before defcustom sets use-ide-diff to t (line 191)
  (setq claude-code-ide-mcp-tools (claude-code-ide-mcp--build-tool-list))
  (setq claude-code-ide-mcp-tool-schemas (claude-code-ide-mcp--build-tool-schemas))
  (setq claude-code-ide-mcp-tool-descriptions (claude-code-ide-mcp--build-tool-descriptions)))

;; emacs-mcp-server — exposes Emacs to Claude Code via MCP unix socket
(use-package mcp-server
  :demand t
  :config
  (mcp-server-start-unix))

;; mgrbyte-claude — external terminal integration and auto-dependency discovery
(require 'mgrbyte-claude)
(mgrbyte-claude-mode 1)

(provide 'init-ai)
;;; init-ai.el ends here
