;;; init-ai.el --- AI/LLM integration -*- lexical-binding: t -*-
;;; Commentary:
;; Configuration for AI assistants: gptel (local LLMs), MCP, Claude Code
;; Packages are installed via Nix (see nix-config/home/packages.nix)
;;; Code:

;; Fix Unicode character width for Claude Code spinner/bullet rendering
;; See: https://github.com/manzaltu/claude-code-ide.el/issues/131
(dolist (range '((#x23FA . #x23FA)  ; ⏺ bullet
                 (#x2500 . #x257F)  ; Box drawing characters
                 (#x2580 . #x259F)  ; Block elements
                 (#x25A0 . #x25FF)  ; Geometric shapes
                 (#x2700 . #x27BF)  ; Dingbats (spinner chars)
                 (#x2200 . #x22FF)  ; Math operators
                 (#x2800 . #x28FF))) ; Braille patterns (progress indicators)
  (set-char-table-range char-width-table range 1))

;; Increase Emacs process read buffer for smoother terminal output
(setq read-process-output-max (* 64 1024))

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

;; claude-code.el - Alternative Claude Code integration (simpler terminal wrapper)
;; Uses C-c C (shift-c) prefix to coexist with claude-code-ide.el on C-c c
(use-package claude-code
  :bind (("C-c C c" . claude-code)
         ("C-c C m" . claude-code-transient)
         ("C-c C k" . claude-code-continue)
         ("C-c C r" . claude-code-resume)
         ("C-c C e" . claude-code-fix-error-at-point)
         ("C-c C q" . claude-code-quit))
  :config
  (setq claude-code-terminal-backend 'vterm))

;; eat - Terminal emulator
(use-package eat)

;; claude-code-ide.el - Claude Code IDE integration
(use-package claude-code-ide
  :demand t
  :bind (("C-c c c" . claude-code-ide)
         ("C-c c m" . claude-code-ide-menu)
         ("C-c c k" . claude-code-ide-continue)
         ("C-c c r" . claude-code-ide-resume)
         ("C-c c t" . claude-code-ide-toggle)
         ("C-c c s" . claude-code-ide-stop)
         ("C-c c d" . claude-code-ide-in-directory)
         ("C-c c R" . claude-code-ide-resume-in-directory))
  :config
  (setq claude-code-ide-terminal-backend 'vterm)
  (setq claude-code-ide-use-side-window nil)
  (setq claude-code-ide-no-flicker t)
  (setq claude-code-ide-vterm-render-delay 0.01)
  (claude-code-ide-emacs-tools-setup)
  ;; Rebuild tool lists — workaround for load-order bug where
  ;; claude-code-ide-mcp-handlers.el builds the tool list at require
  ;; time (line 64) before defcustom sets use-ide-diff to t (line 191)
  (setq claude-code-ide-mcp-tools (claude-code-ide-mcp--build-tool-list))
  (setq claude-code-ide-mcp-tool-schemas (claude-code-ide-mcp--build-tool-schemas))
  (setq claude-code-ide-mcp-tool-descriptions (claude-code-ide-mcp--build-tool-descriptions))
  ;; Add Alt-Enter for newline insertion in claude-code buffers
  (add-hook 'vterm-mode-hook
            (lambda ()
              (when (string-match-p "\\*claude-code\\[" (buffer-name))
                (local-set-key (kbd "M-<return>") #'claude-code-ide-insert-newline)))))

;; vterm-anti-flicker-filter - reduces vterm flickering during rapid redraws
;; Exclude claude-code buffers — claude-code-ide has its own smart renderer
;; that conflicts with this package (both advise vterm--filter).
(use-package vterm-anti-flicker-filter
  :after vterm
  :config
  (add-hook 'vterm-mode-hook
            (lambda ()
              (unless (string-match-p "\\*claude-code" (buffer-name))
                (vterm-anti-flicker-filter-enable)))))


(defun claude-code-ide--open-on-primary (start-fn)
  "Run START-FN then move the claude-code buffer to a new frame on primary display."
  (let ((default-directory (read-directory-name "Project directory: ")))
    (funcall start-fn))
  ;; Find the claude-code buffer and move it to a new frame on primary
  (run-at-time 0.3 nil
               (lambda ()
                 (when-let* ((buf (cl-find-if
                                   (lambda (b)
                                     (string-match-p "\\*claude-code\\[" (buffer-name b)))
                                   (buffer-list)))
                             (new-frame (make-frame)))
                   (select-frame new-frame)
                   (switch-to-buffer buf)
                   (mgrbyte-frame-to-primary-maximized)
                   ;; Delete the window showing this buffer in other frames
                   (dolist (frame (frame-list))
                     (unless (eq frame new-frame)
                       (dolist (win (window-list frame))
                         (when (eq (window-buffer win) buf)
                           (delete-window win)))))))))

(defun claude-code-ide-in-directory ()
  "Start claude-code-ide after prompting for directory, on primary display."
  (interactive)
  (claude-code-ide--open-on-primary #'claude-code-ide))

(defun claude-code-ide-resume-in-directory ()
  "Resume claude-code-ide after prompting for directory, on primary display."
  (interactive)
  (claude-code-ide--open-on-primary #'claude-code-ide-resume))


(use-package mcp-server
  :demand t
  :config
  (mcp-server-start-unix))

(require 'mgrbyte-claude)

(provide 'init-ai)
;;; init-ai.el ends here
