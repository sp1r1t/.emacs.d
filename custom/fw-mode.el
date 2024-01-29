;;; fw-mode.el --- A major mode for free writing -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package topspace)
(use-package wc-mode)

(require 'topspace)
(require 'wc-mode)

(defvar fw-mode-hook nil)

(defvar fw-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'fw-hello)
    map)
  "Keymap for fw major mode.")

(defun fw-hello ()
  "Say hello."
  (interactive)
  (message  "Sup freewriter?!.")
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fw\\'" . fw-mode))

(defun fw-mode ()
  "Major mode for editing fw files."
  (interactive)
  (kill-all-local-variables)
  (use-local-map fw-mode-map)
  (setq major-mode 'fw-mode)
  (setq mode-name "fw")
  (setq required-words 750)

  (setq initial-mood-line (format "  Let your mind flow"))
  (setq mode-line-format initial-mood-line)

  (setq previous-left-margin (car (window-margins)))

  (my/set-left-margin)
  (add-hook 'window-configuration-change-hook 'my/set-left-margin nil t)
  (setq fill-column 80)
  (setq line-spacing 8)
  (auto-fill-mode 1)
  (topspace-mode 1)
  (setq scroll-margin 5)

  (add-hook 'after-change-functions
            (lambda (&rest args)
              (setq wordcount (cadr (wc (point-min) (point-max))))
              (if (> wordcount required-words)
                  (setq mode-line-format (format "  You wrote three pages, Nice! (%dw)" wordcount))
                (setq mode-line-format initial-mood-line ))
              )
            nil t
            )
  (add-hook 'change-major-mode-hook
            (lambda ()
              (my/reset-left-margin previous-left-margin)
              )
            nil t)

  (run-hooks 'fw-mode-hook))

(defun my/window-width () (window-total-width (get-buffer-window (current-buffer))))

(defun my/left-margin()
  "Return the left margin so that an 80 char wide paragraph is centered."
  (/ (- (my/window-width) 80) 2)
  )

(defun my/set-left-margin ()
  "Set the left margin so that an 80 char wide paragraph is centered."
  (interactive)
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) (my/left-margin) 0)
  )

(defun my/reset-left-margin (arg)
  "Reset the left margin to ARG."
  (interactive)
  (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) arg 0)
  )

(setq morning-pages-dir "~/morningpages/")
(defun morningpages()
  "Create a new buffer for morning pages."
  (interactive)
  (setq timestamp (format-time-string "%Y%m%d"))
  (setq filename (concat morning-pages-dir timestamp ".fw"))
  (find-file filename)
  )





(provide 'fw-mode)


;;; fw-mode.el ends here
