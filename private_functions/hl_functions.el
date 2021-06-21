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
  
