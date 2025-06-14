;;; custom-copilot.el --- copilot config  -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:

;;; Code:

(use-package quelpa)

;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))

(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)
(setq quelpa-build-explicit-tar-format-p t)

;; Copilot
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*.el")
                   )
  :hook (
         (org-mode  .  (lambda() (copilot-mode -1)))
         (prog-mode . copilot-mode)
         )

  :config
  (setq copilot-indent-warning-suppress t) ;; old setting
  (setq copilot-indent-offset-warning-disable t) ;; new setting
  (setq copilot-max-char 300000) ;; allow bigger files to be scanned (default is 100000)

  :bind (

         :map
         copilot-mode-map
         ("C-f" . copilot-accept-completion)
         )
  )


(provide 'custom-copilot)
;;; custom-copilot.el ends here
