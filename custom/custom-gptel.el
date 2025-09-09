
;;; custome-gptel.el --- Customizations for org-gptel -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :config
  (define-key gptel-mode-map (kbd "C-c f") 'gptel-toggle-node)
  (setq gptel-default-mode 'org-mode)


  (define-prefix-command 'my-gptel-prefix) ; This defines a new prefix command
  (define-key global-map (kbd "C-.") 'my-gptel-prefix)  ; Set the prefix to 'C-c g'

  (define-key my-gptel-prefix (kbd "c") 'gptel-complete)    ; Bind 'gptel-complete' to 'C-c g c'

  (define-key my-gptel-prefix (kbd "s") 'gptel-send)
  (define-key my-gptel-prefix (kbd "m") 'gptel-menu)      ; Bind 'gptel-reply' to 'C-c g r'
  (define-key my-gptel-prefix (kbd "M") 'gptel-mode)

  (gptel-make-gemini "Gemini" :key "AIzaSyAI9st84i3w8MWmLYm5EG-zhsj5PXH0IR4" :stream t)
  )

;; Note: configure ChatGPT credentials in ~/.authinfo
;; See here https://github.com/karthink/gptel#chatgpt
;; custom-gptel.el ends here
