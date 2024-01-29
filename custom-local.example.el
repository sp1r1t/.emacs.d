;;; custom-local.el --- user customization file    -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

(message "Loading custom-local")

(setq centaur-full-name "Firstname Lastname")
(setq centaur-mail-address "my@email.com")
(setq centaur-org-directory "~/org")
(setq my/centaur-basic-font-size 90)

;;; custom-local.el ends here
