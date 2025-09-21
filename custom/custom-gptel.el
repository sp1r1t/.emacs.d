;;; custom-gptel.el --- Customizations for org-gptel -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package gptel
  :after org
  :config
  (define-key gptel-mode-map (kbd "C-c f") 'gptel-toggle-node)
  (setq gptel-default-mode 'org-mode)


  (defvar my-gptel-prefix)
  (define-prefix-command 'my-gptel-prefix) ; This defines a new prefix command
  (define-key global-map (kbd "C-,") 'my-gptel-prefix)  ; Set the prefix to 'C-c g'

  (define-key my-gptel-prefix (kbd "c") 'gptel-complete)    ; Bind 'gptel-complete' to 'C-c g c'
  (define-key my-gptel-prefix (kbd "s") 'gptel-send)
  (define-key my-gptel-prefix (kbd "m") 'gptel-menu)      ; Bind 'gptel-reply' to 'C-c g r'
  (define-key my-gptel-prefix (kbd "M") 'gptel-mode)

  (gptel-make-gemini "Gemini" :stream t)

  (setq gptel-model 'gpt-5-nano)

  (add-hook 'org-mode-hook 'gptel-mode)

  )

;; Note: configure ChatGPT credentials in ~/.authinfo
;; See here https://github.com/karthink/gptel#chatgpt

(provide 'custom-gptel)
;;; custom-gptel.el ends hereq
