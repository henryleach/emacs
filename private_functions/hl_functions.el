(defun my/simple-add-time ($start-time $dec-add-hours)
  "Given a start time in the format 'HH:MM' returns the time
  x.x hours (decimal) later"
  (format-time-string
   "%F %R"
   (time-add
    (apply
     'encode-time
     (parse-time-string
      (concat
       (format-time-string "%F")
       " "
       $start-time)
      )
     )
    (* $dec-add-hours 60.0 60)
    )
   )
  )

(defun sort-lines-nocase ()
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines))
  )


(defun my/guests-to-attendees ($in-guests &optional $add-check-boxes)
  "Takes a list of copied Google Meet Guests, and turns it into a 
   newline separated list. If add-check-boxes is true, then these
   are added before each name."

  ;; First replace any reversed names that appear in double-quotes
  ;; and split the guests into a list
  (let ((guest-list (split-string
		     (replace-regexp-in-string
			 "\"\\([[:alpha:]]*\\), \\([[:alpha:]]*\\)\""
			 "\\2 \\1"
			 $in-guests)
		     ", ")
		    )
	formatted-list
	split-elt)

    ;; Capitalise the name of each guest, but not their e-mail address
    (dolist (elt guest-list formatted-list)
      (setq split-elt (split-string elt " <"))
      (setq formatted-list (concat formatted-list
				   (if $add-check-boxes "- [ ] ")
				   (concat (capitalize (car split-elt))
					   " <"
					   (cadr split-elt))				   
				   "\n")))

    )
  )

(defun my/int-guest-to-attendees ($start $end)
  "Interactive version of my/guests-to-attendees, defaults
   to adding org-mode check boxes at the start."
  (interactive "r")

  (setq attendee-list (my/guests-to-attendees (buffer-substring-no-properties $start $end) t))

  (save-excursion
    (delete-region $start $end)
    (goto-char $start)
    (insert attendee-list))
  )
  
(defun my/cslist-to-newline-list ($in-cslist)
  "Takes a string of comma separated words, and splits
   them onto newlines, removing the comma."
  (replace-regexp-in-string ", *" "\n" $in-cslist))

;; TODO: There is probably a simple way to 'wrap' these interactive functions
;; to not repeat the same code each time.
(defun my/int-cslist-to-newline-list ($start $end)
  "Interactive version of 'my/cslist-to-newline-list'"
  (interactive "r")

  (setq newline-list (my/cslist-to-newline-list (buffer-substring-no-properties $start $end)))

  (save-excursion
    (delete-region $start $end)
    (goto-char $start)
    (insert newline-list))
  )

(defun my/newline-list-to-cslist ($in-nllist)
  "Takes a list separated by newlines, and puts them
   onto one line, separated by a comma and space."
    (replace-regexp-in-string " *\n" ", " $in-nllist))

(defun my/int-newlins-list-to-cslist ($start $end)
  "Interactive version of 'my/newline-list-to-cslist'"
  (interactive "r")

  (setq newline-list (my/newline-list-to-cslist (buffer-substring-no-properties $start $end)))

  (save-excursion
    (delete-region $start $end)
    (goto-char $start)
    (insert newline-list))
  )

(setq title-replacements-dict
      '(("[-_]" . " ")
        ("\.org" . "")))

(defun my/create-org-title (string)
  "Take and org-mode filename and turn it into a title"
  (capitalize 
  (seq-reduce
   (lambda (string regexp-replacement-pair)
     (replace-regexp-in-string
      (car regexp-replacement-pair)
      (cdr regexp-replacement-pair)
      string))
   title-replacements-dict
   string)))
