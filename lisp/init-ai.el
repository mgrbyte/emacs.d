;;; init-ai.el --- AI/LLM integration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for AI assistants: gptel (local LLMs), MCP, Claude Code
;;; Code:

;; gptel - LLM client (for local models via Ollama, not Claude)
(use-package gptel
  :ensure t
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
  :ensure t
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
  :ensure t
  :after org
  :config
  (setq org-mcp-allowed-files org-agenda-files))

;; claude-code.el - Claude Code CLI integration (uses OAuth, not API key)
(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :bind (("C-c c c" . claude-code)
         ("C-c c k" . claude-code-continue)
         ("C-c c r" . claude-code-resume)
         ("C-c c e" . claude-code-fix-error-at-point)
         ("C-c c q" . claude-code-quit))
  :config
  (setq claude-code-terminal-backend 'eat))

;; eat - Terminal emulator for claude-code
(use-package eat
  :ensure t)

(provide 'init-ai)
;;; init-ai.el ends here
