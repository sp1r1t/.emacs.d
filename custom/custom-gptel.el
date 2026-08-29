;;; custom-gptel.el --- Customizations for org-gptel -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:


(use-package gptel
  :after org
  :config
  (define-key gptel-mode-map (kbd "C-c f") 'gptel-toggle-node)
  (setq gptel-default-mode 'org-mode)


  (defvar my-gptel-prefix)
  (define-prefix-command 'my-gptel-prefix)
  (define-key global-map (kbd "C-,") 'my-gptel-prefix)

  (define-key my-gptel-prefix (kbd "c") 'gptel-complete)
  (define-key my-gptel-prefix (kbd "s") 'gptel-send)
  (define-key my-gptel-prefix (kbd "m") 'gptel-menu)
  (define-key my-gptel-prefix (kbd "M") 'gptel-mode)

  (gptel-make-gemini "Gemini" :stream t :key gptel-api-key)

  (gptel-make-anthropic "Claude"
    :stream t
    :key gptel-api-key
    :models (append '(claude-sonnet-5 claude-opus-5 claude-haiku-4-5-20251001)
                     gptel--anthropic-models))

  (setq gptel-backend (gptel-make-openai-oauth "OpenAI-sub") ;Any name of your choosing
        gptel-model 'gpt-5.4)

  ;; WORKAROUND: as of the 2026-08-26 MELPA snapshot, `gptel--parse-buffer'
  ;; moves point to `point-min' as a side effect and never restores it.
  ;; A second call in the same command loop (e.g. from the header-line's
  ;; token-usage display) then sees a collapsed buffer and sends an empty
  ;; "input", which the API rejects with invalid_request_error. Remove once
  ;; upstream fixes this (https://github.com/karthink/gptel).
  (advice-add 'gptel--parse-buffer :around
              (lambda (orig-fn &rest args) (save-excursion (apply orig-fn args))))

  (add-hook 'org-mode-hook 'gptel-mode)

  )

;; Note: configure ChatGPT/Gemini/Claude credentials in ~/.authinfo
;; See here https://github.com/karthink/gptel#chatgpt
;; Claude key is looked up via auth-source for host "api.anthropic.com",
;; user "apikey" (already present in ~/.authinfo).
;; Switch the active backend/model with `gptel-menu' (C-, m).

(provide 'custom-gptel)
;;; custom-gptel.el ends hereq
