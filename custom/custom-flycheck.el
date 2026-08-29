(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :bind (:map flycheck-mode-map
         ("M-n" . flycheck-next-error) ; optional but recommended error navigation
         ("M-p" . flycheck-previous-error)))
