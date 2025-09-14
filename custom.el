;;; custom.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; Centaur Emacs variables
(setq centaur-server t)                      ; Enable `server-mode' or not: t or nil
(setq centaur-icon t)                        ; Display icons or not: t or nil
(setq centaur-package-archives 'melpa)         ; Package repo: melpa, emacs-cn, bfsu, netease, sjtu, tencent, tuna or ustc
(setq centaur-theme 'night)                     ; Color theme: auto, random, system, default, pro, dark, light, warm, cold, day or night
(setq centaur-completion-style 'minibuffer)    ; Completion display style: minibuffer or childframe
(setq centaur-dashboard t)                   ; Display dashboard at startup or not: t or nil
(setq centaur-lsp 'lsp-mode)                   ; Set LSP client: lsp-mode, eglot or nil
(setq centaur-lsp-format-on-save t)            ; Auto format buffers on save: t or nil
(setq centaur-tree-sitter t)                 ; Enable tree-sitter or not: t or nil. Only available in 29+.
(setq centaur-player t)                        ; Enable players or not: t or nil

(setq my/centaur-basic-font-size 100)       ; Overwrite this in custom-local.el

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Load local config for overriding
(when (file-readable-p (expand-file-name  "custom-local.el" user-emacs-directory))
  (load-file (expand-file-name  "custom-local.el" user-emacs-directory)))



;; Mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;                                    user-mail-address nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; Calendar
;; Set location , then press `S' can show the time of sunrise and sunset
;; (setq calendar-location-name "Chengdu"
;;       calendar-latitude 30.67
;;       calendar-longitude 104.07)

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)

;; Enable proxy
;; (proxy-http-enable)
;; (proxy-socks-enable)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

;; (put 'cl-destructuring-bind 'lisp-indent-function 'defun)
;; (put 'pdf-view-create-image 'lisp-indent-function 'defun)
;; (put 'treemacs-create-theme 'lisp-indent-function 'defun)

(setq my-agenda-files '("7bit.org" "evo.org"
                        "Document.org" "Menschen.org"
                        "Verborgt.org" "WMS.org"
                        "articles.org" "body.org"
                        "cwc-website.org" "emacs.org"
                        "finanzen.org" "haushalt.org"
                        "inbox.org" "job.org"
                        "linuxlog.org" "mind.org"
                        "projects.org" "someday.org"
                        "tickler.org" "wochenmenu.org"))

(setq my-agenda-files-full-path (mapcar (lambda (file) (expand-file-name file centaur-org-directory)) my-agenda-files))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gptel-model "gpt-4")
 '(org-agenda-files my-agenda-files-full-path)
 '(org-html-postamble 'auto)
 '(org-html-postamble-format
   '(("en"
      "<p class=\"author\">Author: %a (%e)</p>\12<p class=\"date\">Date: %d</p>")))
 '(org-html-preamble t)
 '(package-vc-selected-packages
   '((ultra-scroll :vc-backend Git :url "https://github.com/jdtsmith/ultra-scroll"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ediff-current-diff-A ((t (:background "#ffdddd"))))
 '(ediff-current-diff-B ((t (:background "#ddffdd"))))
 '(ediff-current-diff-C ((t (:background "#ddddff"))))
 '(ediff-even-diff-A ((t (:background "#ffffff"))))
 '(ediff-even-diff-B ((t (:background "#ffffff"))))
 '(ediff-odd-diff-A ((t (:background "#eeeeee"))))
 '(ediff-odd-diff-B ((t (:background "#eeeeee")))))

;; OSX/mac config
(setq mac-option-modifier 'meta)
;; (setq mac-function-modifier 'meta)
(setq mac-command-modifier 'super)

;; font
(set-frame-font "Hack Nerd Font-13" nil t)


;; ut8 encoding
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; global key bindings
(define-key global-map (kbd "C-x C-n") 'consult-buffer)

;;; custom.el ends here
