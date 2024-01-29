;;; custom-kotl.el --- My configurations for the fw mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Copyright (C) 2024  Julian Konrath

(load-file (expand-file-name "custom/fw-mode.el" user-emacs-directory))

;; Disable corfu-mode in fw mode to keep the interface clean
;; and distraction free.

(add-hook 'fw-mode-hook (lambda () (corfu-mode -1)))

;;; Author: Julian Konrath <julian.konrath@gmail.com>
;;; Keywords: outlines
;;; custom-fw.el ends here
