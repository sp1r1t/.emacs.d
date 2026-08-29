;;; custome-hyperbole.el --- Customizations for hyperbole -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package hyperbole
  :init
  (hyperbole-mode) ;; ✅ Ensures it's activated
  :config
  ;; Bind the Action Key to s-<return> via Hyperbole's own global minor-mode
  ;; keymap (`hyperbole-mode-map'), not `global-map'. A plain `:bind'/
  ;; `global-set-key' lands in `global-map', which is the lowest-precedence
  ;; keymap and gets shadowed by any major/minor mode binding the same key
  ;; (e.g. `lsp-ui-mode' also claims s-<return> in init-lsp.el).
  (hkey-set-key (kbd "s-<return>") #'hkey-either)
  ;; `emulation-mode-map-alists' is checked before `minor-mode-map-alist',
  ;; so this makes the Action Key win over other minor modes that bind the
  ;; same key too (per Hyperbole's own suggestion in hyperbole.el). Use a
  ;; small dedicated keymap with ONLY s-<return> here, not the full
  ;; `hyperbole-mode-map' -- promoting the whole map would also put
  ;; Hyperbole's other bindings (e.g. C-c RET -> `hui-select-thing') ahead
  ;; of same-key bindings in other packages, such as gptel's C-c RET ->
  ;; `gptel-send'.
  (defvar my-hyperbole-action-key-map (make-sparse-keymap)
    "Minimal keymap giving only the Hyperbole Action Key top emulation priority.")
  (define-key my-hyperbole-action-key-map (kbd "s-<return>") #'hkey-either)
  ;; Drop any prior `hyperbole-mode' entry (e.g. one pointing at the full
  ;; `hyperbole-mode-map' from an earlier version of this file) so reloading
  ;; doesn't leave two conflicting entries active at once.
  (setq emulation-mode-map-alists
        (seq-remove (lambda (entry) (and (consp entry) (eq (caar entry) 'hyperbole-mode)))
                    emulation-mode-map-alists))
  (add-to-list 'emulation-mode-map-alists `((hyperbole-mode . ,my-hyperbole-action-key-map))))

;;; custom-hyperbole.el ends here
