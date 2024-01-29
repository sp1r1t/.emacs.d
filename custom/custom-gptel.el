
;;; custome-gptel.el --- Customizations for org-gptel -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package gptel
  :config
  (define-key gptel-mode-map (kbd "C-c f") 'gptel-toggle-node)
  (setq gptel-default-mode 'org-mode)
  )

;; Note: configure ChatGPT credentials in ~/.authinfo
;; See here https://github.com/karthink/gptel#chatgpt
;;; custom-gptel.el ends here
