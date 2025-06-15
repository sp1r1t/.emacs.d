;; corfu
;; corfu complete on tab
(use-package corfu
  :init
  (global-corfu-mode) ;; ✅ Ensures it's activated
  :config
  ;; Custom keybindings
  (define-key corfu-map [tab] 'corfu-complete)
  (define-key corfu-map [?\t] 'corfu-complete)
  (define-key corfu-map [return] nil)
  (define-key corfu-map [?\r] nil))
