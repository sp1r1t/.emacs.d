(defun my/insert-date (offset)
  "Insert a date like YYYY-MM-DD, with OFFSET days from today.
OFFSET = 0 for today, -1 for yesterday, etc."
  (interactive "p")
  (insert (format-time-string "%Y-%m-%d"
                              (org-read-date nil t nil nil nil nil offset))))
