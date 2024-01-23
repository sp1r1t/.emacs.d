;;; custom-terminal.el --- Terminal configuration
;;; Commentary:
;;; Code:

;; Vterm
(use-package vterm
  :config
  (setq vterm-shell "/bin/zsh")
  )

(use-package vterm-toggle
  :bind (("C-1" . vterm-toggle))
  )

;; Eshell
;; (require 'eshell)
;; (require 'em-smart)
;; (setq eshell-where-to-jump 'begin)
;; (setq eshell-review-quick-commands nil)
;; (setq eshell-smart-space-goes-to-end t)

;;; custom-terminals.el ends here
