;; global essentials:
;; C-c c = org-capture
;; C-c a = org-agenda
;;
;; org-mode essentials:
;; C-c C-w = org-refile
;; C-c t = change state
;; C-c C-s = schedule a todo
;; TAB/S-TAB = cycle through viewing trees
;;
;; agenda view:
;; C-c a = org-agenda
;; ... t = list all todo entries plus file location
;; 
;; clocking essentials (on tasks)
;; C-c C-x TAB = org-clock-in
;; C-c C-x o = org-clock-out


(use-package org
  :diminish org-indent
  :preface
  (defun spa/diminish-org-indent ()
    (interactive)
    (diminish 'org-indent-mode ""))
  :config
  ;; Set the directory for org files
  (setq org-directory '("~/GTD/"))  
  (setq org-agenda-files '("~/GTD/"))
;;  (setq org-archive-location "~/GTD/::* Archived Tasks")
  (setq org-agenda-clock-consistency-checks
	(quote
	 (:max-duration "4:00" :min-duration 0 :max-gap "0:05" :gap-ok-around
			("4:00" "12:30")
			:default-face
			((:background "DarkRed")
			 (:foreground "white"))
			:overlap-face nil :gap-face nil :no-end-time-face nil :long-face nil :short-face nil)))
  ;; visual parameters
  (setq org-cycle-separator-lines 0)
  (setq org-blank-before-entry 0)
  ;; Set format for org duration in hours rather than default days:hours:minutes
  (setq org-duration-format (quote h:mm))
  ;; Set default priority of TODO items to be the same as [#C]
  (setq org-default-priority ?C)
  ;; Set default range from displaying clocks to all time until now
  ;; Use C-u C-u prefix to choose a different range   
  (setq org-clock-display-default-range 'untilnow)
  ;; Set clocktable defaults
  (setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 4 :compact t))
  ;; use cornered arrow instead of elipse for hidden items
  (setq org-ellipsis "⤵")
  ;; hide the leading stars (even when not in indent-mode)
  (setq org-hide-leading-stars t)
  ;; warn me of any deadlines in next 7 days
  (setq org-deadline-warning-days 7)
  ;; don't show tasks as scheduled if they are already shown as a deadline
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  ;; Default notes and agenda files
  (setq org-default-notes-file "~/GTD/notes.org")
  ;; Refile
  (setq org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
  

  ;; org-capture templates
  ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
  (setq org-capture-templates
	(quote (("t" "Todo" entry (file "~/GTD/refile.org")
		 "* TODO %?\n%U\n" :clock-in t :clock-resume t)
		;; ("r" "respond" entry (file "~/GTD/refile.org")
		;;  "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n" :clock-in t :clock-resume t :immediate-finish t)
		("n" "Note" entry (file "~/GTD/refile.org")
		 "* %? :NOTE:\n%U\n" :clock-in t :clock-resume t)
		("j" "Journal" entry (file+datetree "~/GTD/journal.org")
		 "* %?\n%U\n" :clock-in t :clock-resume t)
		;; ("w" "org-protocol" entry (file "~/GTD/refile.org")
		;;  "* TODO Review %c\n%U\n" :immediate-finish t)
		("m" "Meeting" entry (file "~/GTD/refile.org")
		 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
		;; ("p" "Phone call" entry (file "~/GTD/refile.org")
		;;  "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
		("h" "Habit" entry (file "~/GTD/refile.org")
		 "* NEXT %?\n%U\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

  ;; format string used when creating CLOCKSUM lines and when generating a
  ;; time duration (avoid showing days)
  (setq org-time-clocksum-format
	'(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
  ;; Press C-a twice to get to start of heading instead of stars
  (setq org-special-ctrl-a/e 'reversed)
  ;; Capture time stamps when TODO states change
  (setq org-log-done 'time)
  ;; '@' sign means that I want to log a note with time
  ;; stamp when the state changes
  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		;;	(sequence "REPLAY(r)" "|" "REPLAYED(p)")
		(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")
		(sequence "RUNNING(R@)" "ERROR(E@)" "|"  "FINISHED(F@)")	      )))

  ;; set faces of states
  (setq org-todo-keyword-faces
	(quote (("TODO" :foreground "red" :weight bold)
		("NEXT" :foreground "light blue" :weight bold)
		("DONE" :foreground "forest green" :weight bold)
		;;		("REPLAY" :foreground "red" :weight bold)
		;;		("REPLAYED" :foreground "forest green" :weight bold)		
		("WAITING" :foreground "orange" :weight bold)
		("HOLD" :foreground "magenta" :weight bold)
		("CANCELLED" :foreground "forest green" :weight bold)
		("MEETING" :foreground "forest green" :weight bold))))
  ;;		("PHONE" :foreground "forest green" :weight bold))))

  ;; Effort and global properties
  (setq org-global-properties '(("Effort_ALL". "0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 6:00 8:00")))

  ;; Set global Column View format
  (setq org-columns-default-format '"%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM(Clock)")
  
  ;; Prefer a vertical split in Org mode for the agenda
  (defadvice org-agenda (around split-vertically activate)
    (let ((split-width-threshold 80))  ; or whatever width makes sense for you
      ad-do-it))

  (add-to-list 'org-file-apps '(directory . emacs))

  :hook
  (org-indent-mode . spa/diminish-org-indent)
  (org-mode . turn-on-font-lock)
  (org-mode . visual-line-mode)
  (org-agenda-mode . hl-line-mode)

  :bind (:map global-map
	      ("C-c a" . org-agenda)
	      ("C-c c" . org-capture)
	      ("C-c l" . org-store-link))
  (:map org-mode-map
	;; ("C-c b" . org-iswitchb)	; now using ido-mode for buffer switching
	("C-c l" . org-store-link)
	("C-c c" . org-capture)
	("C-c C-f" . org-forward-heading-same-level)
	("C-c s" . 'spa/org-link-screenshot)))

(use-package org-clock-convenience
  :ensure t
  :after (org)
  :init
  (setq org-clock-convenience-clocked-agenda-re "^ +\\([^:]+\\): *\\(\\([ 012][0-9]\\):\\([0-5][0-9]\\)\\)\\(?:-\\(\\([ 012][0-9]\\):\\([0-5][0-9]\\)\\)\\|.*\\)? +Clocked: +\\(([0-9]+:[0-5][0-9])\\|(-)\\)")
  :bind (:map org-agenda-mode-map
   	   ("<S-up>" . org-clock-convenience-timestamp-up)
   	   ("<S-down>" . org-clock-convenience-timestamp-down)
   	   ("o" . org-clock-convenience-fill-gap)
   	   ("e" . org-clock-convenience-fill-gap-both)))

(use-package ox-pandoc
  :ensure t
  :after (org))

(use-package org-bullets
  :ensure t
  :hook (org-mode . (lambda () (org-bullets-mode 1)))
  :after (org))
