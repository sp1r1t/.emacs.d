(defun my/insert-date (offset)
  "Insert a date like YYYY-MM-DD, with OFFSET days from today.
OFFSET = 0 for today, -1 for yesterday, etc."
  (interactive "p")
  (insert (format-time-string "%Y-%m-%d"
                              (org-read-date nil t nil nil nil nil offset))))


(defun color-normalize-hex (color)
  "Convert COLOR like #RRRRGGGGBBBB to #RRGGBB."
  (if (string-match "^#\\([0-9a-fA-F]\\{4\\}\\)\\([0-9a-fA-F]\\{4\\}\\)\\([0-9a-fA-F]\\{4\\}\\)$" color)
      (let ((r (substring (match-string 1 color) 0 2))
            (g (substring (match-string 2 color) 0 2))
            (b (substring (match-string 3 color) 0 2)))
        (concat "#" r g b))
    color))

(defun copy-color-to-kill-ring ()
  "Pick a color with `read-color`, normalize it, and copy to kill ring."
  (interactive)
  (let* ((c (read-color "Color: " t))
         (hex (color-normalize-hex c)))
    (kill-new hex)
    (message "Copied %s to kill ring" hex)))
