;;; custom-claude-code-ide.el --- Customizations for claude-code-ide -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  ;; `exec-path-from-shell-arguments' is set to ("-l") (login, non-interactive),
  ;; so it never sources .zshrc, where ~/.local/bin (claude's install dir) is
  ;; added to PATH -- zsh only reads .zshrc for interactive shells. Point
  ;; straight at the binary rather than relying on PATH resolution.
  (setq claude-code-ide-cli-path (expand-file-name "~/.local/bin/claude"))
  ;; eat is pure Elisp (no native module to compile) and its mode-switching
  ;; (C-c C-e / C-c C-j) gives free scrolling without vterm's auto-scroll
  ;; fighting manual scroll.
  (setq claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-emacs-tools-setup)) ;; Expose Emacs MCP tools (xref, project, etc.)

(provide 'custom-claude-code-ide)
;;; custom-claude-code-ide.el ends here
