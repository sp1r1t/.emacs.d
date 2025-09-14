;; --- Smooth, predictable scrolling ---

;; 1) Use pixel-precision scrolling (Emacs 29+); fall back to pixel-scroll.
(when (boundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))
(when (boundp 'pixel-scroll-mode)
  (pixel-scroll-mode 1))   ;; harmless if precision mode is present

;; 2) Make wheel deltas small and linear; no acceleration.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse t)   ;; scroll the window under the pointer

;; 3) Don’t let scroll-margin yank point around.
;;    If you like a margin, keep it tiny (e.g., 2); 0 is the smoothest.
(setq scroll-margin 0)

;; 4) Avoid recenters and preserve visual position when paging.
(setq scroll-conservatively 101)           ;; never recenter unless necessary
(setq scroll-step 1)                       ;; minimal line movement to keep point on screen
(setq scroll-preserve-screen-position 'always)

;; 5) Disable automatic vscroll heuristics that cause micro-jumps.
(setq auto-window-vscroll nil)

;; Optional: tiny extra breathing room when using PgUp/PgDn
;; (setq next-screen-context-lines 3)

;; Optional (some displays feel snappier):
(setq fast-but-imprecise-scrolling t)


;;; Keep cursor on the same screen line during MOUSE scrolling
;;; (keyboard scrolling remains as usual)
