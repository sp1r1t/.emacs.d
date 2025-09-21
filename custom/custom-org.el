;;; custom-org.el --- Org mode configuration
;;; Commentary:
;;; Code:

(require 'org)

(when org-directory
  (require 'org-agenda)
  (require 'org-capture)
  (require 'org-attach)
  (require 'org-archive)
  (require 'org-refile)

  (message "Loading org-mode config...")

  (defun in ()
    (interactive)
    (find-file (expand-file-name "inbox.org" org-directory)))


  (eval-after-load 'org
    '(progn
       (setq org-agenda-files (list
                               (symbol-value 'org-directory)
                               ;; (expand-file-name "tmpagenda" (symbol-value 'org-directory))
                               ))
       (setq org-refile-targets
             `(
               (nil :maxlevel . 4)
               (org-agenda-files :maxlevel . 4)
               (,(directory-files-recursively (expand-file-name "reference" org-directory) "^[[:alnum:]-]*.org$") :maxlevel . 1)
               ))
       ;; (setq org-refile-targets
       ;;       '((nil :maxlevel . 3)
       ;;         (org-agenda-files :maxlevel . 3)))

       (setq org-refile-use-outline-path 'file)
       (setq org-outline-path-complete-in-steps nil)

       (setq org-todo-keywords '((sequence  "🎈(t)" "🔥(N)" "▶️(o)" "☎️(C)" "⏳(w)" "🫴(D)" "🚀(p)" "🔄(r)" "📜(n)" "💡(i)" "❔(m)" "✏️(s)"  "|" "✔️(d)" "❌(c)" )))

       (setq org-capture-templates
             `(("t" "🎈" entry (file ,(concat org-directory "/inbox.org"))
                "* 🎈 %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)

               ("i" "💡" entry (file ,(concat org-directory "/inbox.org"))
                "* 💡 %^{Title} %?\n%U\n%a\n")

               ("n" "📜" entry (file ,(concat org-directory "/inbox.org"))
                "* 📜 %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)

               ("j" "Journal" entry (file+olp+datetree
                                     ,(concat org-directory "/journal.org"))
                "*  %^{Title} %?\n%U\n%a\n" :clock-in t :clock-resume t)
	           ("b" "Book" entry (file+olp+datetree
                                  ,(concat org-directory "/book.org"))
	            "* Topic: %^{Description}  %^g %? Added: %U")))

       ;; Preserve window setup when opening angend
       (setq org-agenda-window-setup 'current-window)
       ))


  ;; archive
  (setq org-archive-location "archive/%s_archive::")

  ;; org attach
  ;; add the attachment directory to org-link, so it can be inserted fast with SPC m l l
  (setq org-attach-store-link-p 'attached)

  ;; Todo state dependencies
  (setq org-enforce-todo-dependencies 't)
  (setq org-enforce-todo-checkbox-dependencies 't)

  ;; Archive hierarchical
  (setq org-archive-subtree-hierarchical-file "lib/org-archive-subtree-hierarchical.el")
  (load-file (expand-file-name org-archive-subtree-hierarchical-file
                               user-emacs-directory))

  (defun org-archive-subtree-hierarchical-and-save ()
    (interactive)
    (org-archive-subtree-hierarchical)
    (save-buffer)
    (message "Subtree archived"))

  (setq org-archive-default-command 'org-archive-subtree-hierarchical-and-save)


  ;; shortcut to archive all DONE tasks
  (defun org-archive-done-tasks()
    "Archive all done tasks in the current buffer (emojis as todo state supported).
Use `org-archive-subtree-default` to archive all tasks with a
todo state mathing `org-done-keywords`. This is a new
implementation that uses `string-match` to compare the keywords,
because the original matcher of `org-map-entries` does not work
with emoji characters."
    (interactive)
    (let ((matching-entries ()))
      (org-map-entries (lambda()
                         (if
                             (and
                              (org-entry-get nil "TODO")
                              (string-match (concat "\\(" (regexp-opt org-done-keywords) "\\)") (org-entry-get nil "TODO")))
                             (progn
                               (org-archive-subtree-default)
                               (setq org-map-continue-from (org-element-property :begin
                                                             (org-element-at-point)))
                               )
                           )
                         ) "/" 'file)
      ))



  ;; turn on auto saving
  (add-hook 'auto-save-hook 'org-save-all-org-buffers)

  ;; Open links behaviour
  ;; https://stackoverflow.com/questions/17590784/how-to-let-org-mode-open-a-link-like-file-file-org-in-current-window-inste
  (setq org-link-frame-setup
        '((vm . vm-visit-folder-other-frame)
          (vm-imap . vm-visit-imap-folder-other-frame)
          (gnus . org-gnus-no-new-news)
          (file . find-file)
          (wl . wl-other-frame)
          )
        )


  ;; Set export directory
  (defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
    (unless pub-dir
      (setq pub-dir "exports")
      (unless (file-directory-p pub-dir)
        (make-directory pub-dir)))
    (apply orig-fun extension subtreep pub-dir nil))
  (advice-add 'org-export-output-file-name
              :around #'org-export-output-file-name-modified)


  ;; Org download
  (use-package org-download
    :after org
    :config
    (setq-default org-download-image-dir "./images")
    ;; Drag-and-drop to `dired`
    (add-hook 'dired-mode-hook 'org-download-enable)
    )

  ;; Latex
  (setq org-latex-compiler "pdflatex")

  (setq org-latex-classes '(("\\setmainfont{DejaVu Serif}
    \\setsansfont[Scale=MatchLowercase]{DejaVu Sans}
    \\setmonofont[Scale=MatchLowercase]{DejaVu Sans Mono}")
                            ("article" "\\documentclass[11pt]{article}
    \\usepackage{sectsty}
    \\allsectionsfont{\\sffamily}

    \\usepackage{titling}
    \\pretitle{\\begin{center}\\Huge\\sffamily\\bfseries}
    \\posttitle{\\par\\end{center}}
    \\preauthor{\\begin{center}\\large\\sffamily}
    \\postauthor{\\par\\end{center}}
    \\predate{\\begin{center}\\small\\sffamily}
    \\postdate{\\par\\end{center}}

    \\usepackage{xcolor}
    \\renewcommand\\labelitemi{{\\color{gray}\\circ}}
    \\renewcommand\\labelitemii{{\\color{gray}--}}

    \\usepackage[a4paper,margin=1.2in]{geometry}
   " ("\\section{%s}" . "\\section*{%s}")
   ("\\subsection{%s}" . "\\subsection*{%s}")
   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
   ("\\paragraph{%s}" . "\\paragraph*{%s}")
   ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                            ("report" "\\documentclass[11pt]{report}" ("\\part{%s}" . "\\part*{%s}")
                             ("\\chapter{%s}" . "\\chapter*{%s}")
                             ("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                            ("book" "\\documentclass[11pt]{book}" ("\\part{%s}" . "\\part*{%s}")
                             ("\\chapter{%s}" . "\\chapter*{%s}")
                             ("\\section{%s}" . "\\section*{%s}")
                             ("\\subsection{%s}" . "\\subsection*{%s}")
                             ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

  ;; Default export to subtree
  (setq org-export-initial-scope 'subtree)

  ;; Turn off TOC in exports by default
  (setq org-export-with-toc nil)

  ;; Turn off section numbers by defaultq
  (setq org-export-with-section-numbers nil)

  ;; Image preview
  (setq org-image-actual-width 600)

  ;; Org modern customization
  (setq org-modern-todo nil) ;; make it compatible with emoji todo states

  ;; Super agenda
  (use-package org-super-agenda)

  (eval-after-load 'org-agenda
    '(progn
       (when (package-installed-p 'org-super-agenda)
         (org-super-agenda-mode)
         (setq org-agenda-skip-scheduled-if-done t
               org-agenda-skip-deadline-if-done t
               org-agenda-include-deadlines t
               org-agenda-include-diary t
               org-agenda-block-separator nil
               org-agenda-compact-blocks t
               org-agenda-start-with-log-mode t
               org-agenda-start-day nil)
         (setq org-agenda-span 'week)

         ;; Custimise agenda view
         (setq org-agenda-custom-commands
               '(("u" "Super view"
                  ((agenda "" ( (org-agenda-span 1)
                                (org-super-agenda-groups
                                 '(
                                   (:auto-category t)
                                   ))))
                   ))
                 ("f" "Forward looking"
                  ((agenda "" ((org-super-agenda-groups
                                '((:log t)  ; Automatically named "Log"
                                  (:name "Schedule"
                                   :time-grid t)
                                  (:name "Today"
                                   :scheduled today)
                                  (:habit t)
                                  (:name "Due today"
                                   :deadline today)
                                  (:name "Overdue"
                                   :deadline past)
                                  (:name "Due soon"
                                   :deadline future)
                                  ;; :todo (org-todo-keywords)
                                  (:order 100)
                                  )
                                (:name "Waiting..."
                                 :todo "WAITING"
                                 :order 98)
                                (:name "Scheduled earlier"
                                 :scheduled past)))))
                  )
                 ("d" "Super default"
                  ((agenda "" ((org-super-agenda-groups
                                '(;; Each gras an implicit boolean OR operator between its selec
                                  (:name "Today"  ; Optionally specify section name
                                   :time-grid t  ; Items that appear on the time grid

                                   (:name "Important"
                                    ;; Single arguments given alone
                                    :tag "bills"
                                    :priority "A")
                                   ;; Set order of multiple groups at once
                                   (:order-multi (2 (:name "Shopping in town"
                                                     ;; Boolean AND group matches items that match all subgroups
                                                     :and (:tag "shopping" :tag "@town"))
                                                    (:name "Food-related"
                                                     ;; Multiple args given in list with implicit OR
                                                     :tag ("food" "dinner"))
                                                    (:name "Personal"
                                                     :habit t
                                                     :tag "personal")
                                                    (:name "Space-related (non-moon-or-planet-related)"
                                                     ;; Regexps match case-insensitively on the entire entry
                                                     :and (:regexp ("space" "NASA")
                                                           ;; Boolean NOT also has implicit OR between selectors
                                                           :not (:regexp "moon" :tag "planet")))))
                                   ;; Groups supply their own section names when none are given
                                   (:todo "WAITING" :order 8)  ; Set order of this section
                                   (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                                    ;; Show this group at the end of the agenda (since it has the
                                    ;; highest number). If you specified this group last, items
                                    ;; with these todo keywords that e.g. have priority A would be
                                    ;; displayed in that group instead, because items are grouped
                                    ;; out in the order the groups are listed.
                                    :order 9)
                                   (:priority<= "B"
                                    ;; Show this section after "Today" and "Important", because
                                    ;; their order is unspecified, defaulting to 0. Sections
                                    ;; are displayed lowest-number-first.
                                    :order 1)
                                   ;; After the last group, the agenda will display items that didn't
                                   ;; match any of these groups, with the default order position of 99
                                   ))))))
                 )
                 ("w" "Agenda and TODOs from work"
                  ((agenda "" ((org-super-agenda-groups
                                '((:log t)  ; Automatically named "Log"
                                  (:name "Schedule"
                                   :time-grid t)
                                  (:name "Today"
                                   :scheduled today)
                                  (:habit t)
                                  (:name "Due today"
                                   :deadline today)
                                  (:name "Overdue"
                                   :deadline past)
                                  (:name "Due soon"
                                   :deadline future)
                                  ;; :todo (org-todo-keywords)
                                  (:order 100)
                                  )
                                (:name "Waiting..."
                                 :todo "WAITING"
                                 :order 98)
                                (:name "Scheduled earlier"
                                 :scheduled past))))
                   )
                  ((org-agenda-files '( "~/org/evo.org")))
                  ))
               )
         )
       )
    )

  ;; My org functions
  (defun my/org-file-cleanup()
    "Archive done tasks and sort TODO entries by state on file level."
    (interactive)
    (org-archive-done-tasks)
    (mark-whole-buffer)
    (org-sort-entries nil ?o)
    (org-fold-hide-sublevels 1))

  (defun my/org-file-cleanup-remote(file)
    (setq isOpen (find-buffer-visiting file))
    (with-current-buffer (find-file file)
      (progn
        (my/org-file-cleanup)
        (save-buffer)
        (unless isOpen
          (kill-buffer)))
      ;; (progn
      ;;(message "is open: %s" (current-buffer))
      ;; (kill-buffer)))
      )
    )

  ;; (defun save-org-on-focus-out ()
  ;;   "Save the current buffer if it is visiting an .org file."
  ;;   (when (and buffer-file-name
  ;;              (string-suffix-p ".org" buffer-file-name))
  ;;     (save-buffer)))

  ;; (add-hook 'focus-out-hook 'save-org-on-focus-out)


  ;; Core Function fixes

  ;; Prevent deletion of other windows when canging TODO state (org-todo)
  (defun my/org-fast-todo-selection (&optional current-state)
    "Fast TODO keyword selection with single keys.
Returns the new TODO keyword, or nil if no state change should occur.
  When CURRENT-STATE is given and selection letters are not unique globally,
  prefer a state in the current sequence over on in another sequence."
    (let* ((fulltable org-todo-key-alist)
           (head (org-get-todo-sequence-head current-state))
           (done-keywords org-done-keywords) ;; needed for the faces.
           (maxlen (apply 'max (mapcar
                                (lambda (x)
                                  (if (stringp (car x)) (string-width (car x)) 0))
                                fulltable)))
           (expert (equal org-use-fast-todo-selection 'expert))
           (prompt "")
           (fwidth (+ maxlen 3 1 3))
           (ncol (/ (- (window-width) 4) fwidth))
           tg cnt e c tbl subtable
           groups ingroup in-current-sequence)
      (save-excursion
        (save-window-excursion
          (if expert
              (set-buffer (get-buffer-create " *Org todo*"))
            ;;(delete-other-windows)
            (set-window-buffer (split-window-vertically) (get-buffer-create " *Org todo*"))
            (org-switch-to-buffer-other-window " *Org todo*"))
          (erase-buffer)
          (setq-local org-done-keywords done-keywords)
          (setq tbl fulltable cnt 0)
          (while (setq e (pop tbl))
            (cond
             ((equal e '(:startgroup))
              (push '() groups) (setq ingroup t)
              (unless (= cnt 0)
                (setq cnt 0)
                (insert "\n"))
              (setq prompt (concat prompt "{"))
              (insert "{ "))
             ((equal e '(:endgroup))
              (setq ingroup nil cnt 0 in-current-sequence nil)
              (setq prompt (concat prompt "}"))
              (insert "}\n"))
             ((equal e '(:newline))
              (unless (= cnt 0)
                (setq cnt 0)
                (insert "\n")
                (setq e (car tbl))
                (while (equal (car tbl) '(:newline))
                  (insert "\n")
                  (setq tbl (cdr tbl)))))
             (t
              (setq tg (car e) c (cdr e))
              (if (equal tg head) (setq in-current-sequence t))
              (when ingroup (push tg (car groups)))
              (when in-current-sequence (push e subtable))
              (setq tg (org-add-props tg nil 'face
                                      (org-get-todo-face tg)))
              (when (and (= cnt 0) (not ingroup)) (insert "  "))
              (setq prompt (concat prompt "[" (char-to-string c) "] " tg " "))
              (insert "[" c "] " tg (make-string
                                     (- fwidth 4 (length tg)) ?\ ))
              (when (and (= (setq cnt (1+ cnt)) ncol)
                         ;; Avoid lines with just a closing delimiter.
                         (not (equal (car tbl) '(:endgroup))))
                (insert "\n")
                (when ingroup (insert "  "))
                (setq cnt 0)))))
          (insert "\n")
          (goto-char (point-min))
          (unless expert (org-fit-window-to-buffer))
          (message (concat "[a-z..]:Set [SPC]:clear"
                           (if expert (concat "\n" prompt) "")))
          (setq c (let ((inhibit-quit t)) (read-char-exclusive)))
          (setq subtable (nreverse subtable))
          (cond
           ((or (= c ?\C-g)
                (and (= c ?q) (not (rassoc c fulltable))))
            (setq quit-flag t))
           ((= c ?\ ) nil)
           ((setq e (or (rassoc c subtable) (rassoc c fulltable))
                  tg (car e))
            tg)
           (t (setq quit-flag t)))))))

  (advice-add 'org-fast-todo-selection :override #'my/org-fast-todo-selection)


  (defun my/org-agenda-get-restriction-and-command (prefix-descriptions)
    "The user interface for selecting an agenda command."
    (catch 'exit
      (let* ((bfn (buffer-file-name (buffer-base-buffer)))
	         (restrict-ok (and bfn (derived-mode-p 'org-mode)))
	         (region-p (org-region-active-p))
	         (custom org-agenda-custom-commands)
	         (selstring "")
	         restriction second-time
	         c entry key type match prefixes rmheader header-end custom1 desc
	         line lines left right n n1)
        (save-window-excursion
	      ;;(delete-other-windows)
	      (org-switch-to-buffer-other-window " *Agenda Commands*")
	      (erase-buffer)
	      (insert (eval-when-compile
		            (let ((header
			               (copy-sequence
			                "Press key for an agenda command:
--------------------------------        <   Buffer, subtree/region restriction
a   Agenda for current week or day      >   Remove restriction
t   List of all TODO entries            e   Export agenda views
m   Match a TAGS/PROP/TODO query        T   Entries with special TODO kwd
s   Search for keywords                 M   Like m, but only TODO entries
/   Multi-occur                         S   Like s, but only TODO entries
?   Find :FLAGGED: entries              C   Configure custom agenda commands
*   Toggle sticky agenda views          #   List stuck projects (!=configure)
"))
			              (start 0))
		              (while (string-match
			                  "\\(^\\|   \\|(\\)\\(\\S-\\)\\( \\|=\\)"
			                  header start)
		                (setq start (match-end 0))
		                (add-text-properties (match-beginning 2) (match-end 2)
					                         '(face bold) header))
		              header)))
	      (setq header-end (point-marker))
	      (while t
	        (setq custom1 custom)
	        (when (eq rmheader t)
	          (org-goto-line 1)
	          (re-search-forward ":" nil t)
              (delete-region (match-end 0) (line-end-position))
	          (forward-char 1)
	          (looking-at "-+")
              (delete-region (match-end 0) (line-end-position))
	          (move-marker header-end (match-end 0)))
	        (goto-char header-end)
	        (delete-region (point) (point-max))

	        ;; Produce all the lines that describe custom commands and prefixes
	        (setq lines nil)
	        (while (setq entry (pop custom1))
	          (setq key (car entry) desc (nth 1 entry)
		            type (nth 2 entry)
		            match (nth 3 entry))
	          (if (> (length key) 1)
		          (cl-pushnew (string-to-char key) prefixes :test #'equal)
	            (setq line
		              (format
		               "%-4s%-14s"
		               (org-add-props (copy-sequence key)
			               '(face bold))
		               (cond
		                ((string-match "\\S-" desc) desc)
		                ((eq type 'agenda) "Agenda for current week or day")
		                ((eq type 'agenda*) "Appointments for current week or day")
		                ((eq type 'alltodo) "List of all TODO entries")
		                ((eq type 'search) "Word search")
		                ((eq type 'stuck) "List of stuck projects")
		                ((eq type 'todo) "TODO keyword")
		                ((eq type 'tags) "Tags query")
		                ((eq type 'tags-todo) "Tags (TODO)")
		                ((eq type 'tags-tree) "Tags tree")
		                ((eq type 'todo-tree) "TODO kwd tree")
		                ((eq type 'occur-tree) "Occur tree")
		                ((functionp type) (if (symbolp type)
					                          (symbol-name type)
					                        "Lambda expression"))
		                (t "???"))))
	            (cond
	             ((not (org-string-nw-p match)) nil)
	             (org-agenda-menu-show-matcher
		          (setq line
		                (concat line ": "
			                    (cond
			                     ((stringp match)
				                  (propertize match 'face 'org-warning))
			                     ((listp type)
				                  (format "set of %d commands" (length type)))))))
	             (t
		          (org-add-props line nil 'help-echo (concat "Matcher: " match))))
	            (push line lines)))
	        (setq lines (nreverse lines))
	        (when prefixes
	          (mapc (lambda (x)
		              (push
		               (format "%s   %s"
			                   (org-add-props (char-to-string x)
				                   nil 'face 'bold)
			                   (or (cdr (assoc (concat selstring
						                               (char-to-string x))
					                           prefix-descriptions))
				                   "Prefix key"))
		               lines))
		            prefixes))


	        (if org-agenda-menu-two-columns
	            (progn
		          (setq n (length lines)
		                n1 (+ (/ n 2) (mod n 2))
		                right (nthcdr n1 lines)
		                left (copy-sequence lines))
		          (setcdr (nthcdr (1- n1) left) nil))
	          (setq left lines right nil))
	        (while left
	          (insert "\n" (pop left))
	          (when right
	            (if (< (current-column) 40)
		            (move-to-column 40 t)
		          (insert "   "))
	            (insert (pop right))))

	        ;; Make the window the right size
	        (goto-char (point-min))
	        (if second-time
	            (when (not (pos-visible-in-window-p (point-max)))
		          (org-fit-window-to-buffer))
	          (setq second-time t)
	          (org-fit-window-to-buffer))

	        ;; Hint to navigation if window too small for all information
	        (setq header-line-format
		          (when (not (pos-visible-in-window-p (point-max)))
		            "Use C-v, M-v, C-n or C-p to navigate."))

	        ;; Ask for selection
	        (cl-loop
	         do (progn
		          (message "Press key for agenda command%s:"
			               (if (or restrict-ok org-agenda-overriding-restriction)
			                   (if org-agenda-overriding-restriction
				                   " (restriction lock active)"
			                     (if restriction
				                     (format " (restricted to %s)" restriction)
				                   " (unrestricted)"))
			                 ""))
		          (setq c (read-char-exclusive)))
	         until (not (memq c '(14 16 22 134217846)))
	         do (org-scroll c))

	        (message "")
	        (cond
	         ((assoc (char-to-string c) custom)
	          (setq selstring (concat selstring (char-to-string c)))
	          (throw 'exit (cons selstring restriction)))
	         ((memq c prefixes)
	          (setq selstring (concat selstring (char-to-string c))
		            prefixes nil
		            rmheader (or rmheader t)
		            custom (delq nil (mapcar
				                      (lambda (x)
				                        (if (or (= (length (car x)) 1)
					                            (/= (string-to-char (car x)) c))
					                        nil
					                      (cons (substring (car x) 1) (cdr x))))
				                      custom))))
	         ((eq c ?*)
	          (call-interactively 'org-toggle-sticky-agenda)
	          (sit-for 2))
	         ((and (not restrict-ok) (memq c '(?1 ?0 ?<)))
	          (message "Restriction is only possible in Org buffers")
	          (ding) (sit-for 1))
	         ((eq c ?1)
	          (org-agenda-remove-restriction-lock 'noupdate)
	          (setq restriction 'buffer))
	         ((eq c ?0)
	          (org-agenda-remove-restriction-lock 'noupdate)
	          (setq restriction (if region-p 'region 'subtree)))
	         ((eq c ?<)
	          (org-agenda-remove-restriction-lock 'noupdate)
	          (setq restriction
		            (cond
		             ((eq restriction 'buffer)
		              (if region-p 'region 'subtree))
		             ((memq restriction '(subtree region))
		              nil)
		             (t 'buffer))))
	         ((eq c ?>)
	          (org-agenda-remove-restriction-lock 'noupdate)
	          (setq restriction nil))
	         ((and (equal selstring "") (memq c '(?s ?S ?a ?t ?m ?L ?C ?e ?T ?M ?# ?! ?/ ??)))
	          (throw 'exit (cons (setq selstring (char-to-string c)) restriction)))
             ((and (> (length selstring) 0) (eq c ?\d))
              (delete-window)
              (org-agenda-get-restriction-and-command prefix-descriptions))

	         ((equal c ?q) (user-error "Abort"))
	         (t (user-error "Invalid key %c" c))))))))

  (advice-add 'org-agenda-get-restriction-and-command :override #'my/org-agenda-get-restriction-and-command)

  )

;; org clock
(defun my/insert-custom-clock-entry ()
  (interactive)
  (insert "CLOCK: ")
  (org-time-stamp-inactive)
  (insert "--")
  ;; Inserts the current time by default.
  (let ((current-prefix-arg '(4))) (call-interactively 'org-time-stamp-inactive))
  (org-ctrl-c-ctrl-c))

;; org babel
(setq org-babel-default-header-args
      '((:results . "replace verbatim output")))

;; Make JS blocks non-session by default
(with-eval-after-load 'ob-js
  (setq org-babel-default-header-args:js
        (let ((alist org-babel-default-header-args:js))
          (assq-delete-all :session alist)
          (push '(:session . "none") alist)
          ;; optional but nice defaults:
          (assq-delete-all :results alist)
          (push '(:results . "output") alist)
          alist)))

(org-babel-do-load-languages 'org-babel-load-languages '((js . t)))

;; Convenience Functions
(defun insert-week-of-year ()
  "Insert the current week of the year."
  (interactive)
  (let ((week-number (format-time-string "%U")))  ;; %U is the week of the year starting from Sunday
    (insert week-number)))

;; custom keybindings
(global-set-key (kbd "C-c l s") 'org-store-link)

(keymap-unset org-mode-map "C-,")
(keymap-unset org-mode-map "C-'")


;; don't evaluate babel on save
(setq org-export-babel-evaluate nil)

;; fallback font
(set-fontset-font t 'unicode "Noto Sans Symbols 2" nil 'prepend)


;;; custom-org.el ends here
