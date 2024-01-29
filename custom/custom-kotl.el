;;; custom-kotl.el --- My configurations for the koutliner mode  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Copyright (C) 2023  Julian Konrath

;; Disable corfu-mode in koutliner mode to keep the interface clean
;; and distraction free.
(require 'corfu)
(add-hook 'kotl-mode-hook (lambda () (corfu-mode -1)))

;;; Author: Julian Konrath <julian.konrath@gmail.com>
;;; Keywords: outlines
;;; custom-kotl.el ends here
