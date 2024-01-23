;;; custome-origami.el --- Customizations for org-origami -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package origami
  :bind
  (:map origami-mode-map ("C-c f" . origami-toggle-node))

  :config
  (global-origami-mode)
  )

;;; custom-origami.el ends here
