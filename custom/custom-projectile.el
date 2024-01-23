;;; custom-projectile.el --- Projectile configuration
;;; Commentary:
;;; Code:

(use-package projectile
  :init
  (setq projectile-indexing-method 'hybrid)

  :config
  (projectile-mode +1)

  ;; https://github.com/jwiegley/use-package#binding-to-keymaps
  :bind-keymap
  ("C-c p" . projectile-command-map)
  )

;;; custom-projectile.el ends here
