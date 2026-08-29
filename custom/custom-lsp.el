;; Put this in your config
(with-eval-after-load 'lsp-mode
  ;; Don’t prompt for project root—just guess it
  (setq lsp-auto-guess-root t))

;; TRAMP's default tramp-remote-path is a minimal hardcoded list (/bin,
;; /usr/bin, ...) that doesn't inherit the remote shell's $PATH, so
;; `executable-find' over TRAMP can't see an already-installed language
;; server and lsp-mode re-prompts to install it on every file open.
;; https://github.com/emacs-lsp/lsp-mode/issues/4252
(with-eval-after-load 'tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
