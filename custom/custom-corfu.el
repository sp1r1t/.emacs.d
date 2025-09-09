;;; custome-corfu.el --- Customizations for corfu -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package corfu
  :init
  (global-corfu-mode) ;; ✅ Ensures it's activated
  :config
  ;; enable corfu completion on tab
  (define-key corfu-map [tab] 'corfu-complete)
  (define-key corfu-map [?\t] 'corfu-complete)

  ;; disable corfu complete on enter
  (define-key corfu-map [return] nil)
  (define-key corfu-map [?\nil] r)
  )


;;; custom-corfu.el ends here
