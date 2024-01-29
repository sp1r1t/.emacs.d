;;; custome-roam.el --- Customizations for org-roam -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package org-roam
  :config
  (setq org-roam-directory  (expand-file-name "roam" org-directory))
  (org-roam-db-autosync-mode)
  )

;;; custom-roam.el ends here
