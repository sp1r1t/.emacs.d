;;; custom-post.el --- user customization file ran after the centaur init  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom-post.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; Keybindings
(keymap-global-set "C-;" 'comment-line)

;; Auto revert
(global-auto-revert-mode 1)

;; Gracefully shut down emacs server
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;; Auto save directly to file
;; (auto-save-visited-mode t)

;; Eye Candy
(setq line-spacing 3)


;; Magit
(global-set-key (kbd "C-c M") 'magit)

;; Window Management
(global-set-key (kbd "C-2") 'view-echo-area-messages)
(global-set-key (kbd "<backtab>") 'ace-window)


;; Load custom configs
(dolist (file  (directory-files (concat user-emacs-directory "/custom") t "custom.*\\.el$"))
  (load-file file))



;; Corfu mode
;; only expand on RET
(define-key corfu-map (kbd "TAB") nil)
(define-key corfu-map (kbd "RET") 'corfu-insert)

;; Scroll Margin
(setq scroll-margin 5)

;; Primary selection
(setq mouse-drag-copy-region t)
(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; Load path (fix load path on OSX)
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

;; Sort lines case insensitive
(defun sort-lines-nocase ()
  (interactive)
  (setq sort-fold-case t)
  (call-interactively 'sort-lines))

;;(load-file "~/.emacs.d/test.el")

;;; custom-post.el ends here
