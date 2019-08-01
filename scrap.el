;; ;; Dired package - file explorer ------------------------------------------------
;; (use-package dired
;;   :diminish dired-omit-mode
;;   :preface
;;   (defun spa/dired-find-file-follow-symlinks ()
;;     "In Dired, visit the file or directory on the line, following symlinks
;; Source: `https://emacs.stackexchange.com/questions/41286/follow-symlinked-directories-in-dired'"
;;     (interactive)
;;     (let ((find-file-visit-truename t))
;;       (dired-find-file)))
;;   (defun dired-do-ispell (&optional arg)
;;     "Check multiple buffers or files marked in dired for spelling with ispell"
;;     (interactive "P")
;;     (dolist (file (dired-get-marked-files
;;                    nil arg
;;                    #'(lambda (f)
;;                        (not (file-directory-p f)))))
;;       (save-window-excursion
;; 	(with-current-buffer (find-file file)
;;           (ispell-buffer)))
;;       (message nil)))
;;   (defun spa/dired-open-externally ()
;;     "Run `open` on Mac OS X to open the file externally (e)."
;;     (interactive)
;;     (dired-do-shell-command "open" nil (dired-get-marked-files)))
;;   (defun spa/tags-query-replace (from to &optional delimited file-list-form)
;;     "Do `query-replace' of FROM with TO on all files listed in tags table.                     
;; Third arg DELIMITED (prefix arg) means replace only word-delimited matches.                  
;; If you exit (\\[keyboard-quit], RET or q), you can resume the query replace                  
;; with the command \\[tags-loop-continue].                                                     
;; Fourth arg FILE-LIST-FORM non-nil means initialize the replacement loop.                     
;; Fifth and sixth arguments START and END are accepted, for compatibility                      
;; with `query-replace', and ignored.                                                           
;; If FILE-LIST-FORM is non-nil, it is a form to evaluate to
;; produce the list of files to search.
;; See also the documentation of the variable `tags-file-name'.
;; Source: `http://stackoverflow.com/questions/15038277/find-and-replace-without-regexp-in-dired'"
;;     (interactive (query-replace-read-args "Tags query replace" nil t))
;;     (require 'etags)
;;     (setq tags-loop-scan `(let ,(unless (equal from (downcase from))
;;                                   '((case-fold-search nil)))
;;                             (if (search-forward ',from nil t)
;; 				;; When we find a match, move back
;; 				;; to the beginning of it so perform-replace
;; 				;; will see it.
;; 				(goto-char (match-beginning 0))))
;;           tags-loop-operate `(perform-replace ',from ',to t nil ',delimited
;;                                               nil multi-query-replace-map))
;;     (tags-loop-continue (or file-list-form t)))
;; (defun spa/dired-do-query-replace (from to &optional delimited)
;;   "Do `query-replace' of FROM with TO, on all marked files (no regexp).
;; Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
;; If you exit (\\[keyboard-quit], RET or q), you can resume the query replace
;; with the command \\[tags-loop-continue].
;; Source: `http://stackoverflow.com/questions/15038277/find-and-replace-without-regexp-in-dired'"
  
;;   (interactive
;;    (let ((common
;;           (query-replace-read-args
;;            "Query replace in marked files" nil t)))
;;      (list (nth 0 common) (nth 1 common) (nth 2 common))))
;;   (require 'dired-aux)
;;   (dolist (file (dired-get-marked-files nil nil 'dired-nondirectory-p))
;;     (let ((buffer (get-file-buffer file)))
;;       (if (and buffer (with-current-buffer buffer
;;                         buffer-read-only))
;;           (error "File `%s' is visited read-only" file))))
;;   (spa/tags-query-replace
;;    from to delimited '(dired-get-marked-files nil nil 'dired-nondirectory-p)))
;;   :config
;;   ;; To copy from one dired dir to another shown in split window
;;   (setq dired-dwim-target t)
;;   (setq delete-by-moving-to-trash t)
;;   ;; Can use `C-u s` in dired to modify switches
;;   ;; -S option to sort by file size, largest first
;;   ;; -X option to sort alphabetically by extension
;;   (setq dired-listing-switches "-AlhG")
;;   :bind (:map dired-mode-map
;; 	      ("<C-return>" . spa/dired-find-file-follow-symlinks)
;; 	      ("C-c s" . dired-do-ispell)
;; 	      ("e" . spa/dired-open-externally)
;; 	      ("C-c Q" . spa/dired-do-query-replace)))


;; ;; TRAMP for SSH connections
;; (use-package tramp
;;   :defer 5
;;   :preface
;;   (defun spa/find-file-biostat ()
;;   "Calls find-file on filename on remove server using TRAMP.
;; Requires a password"
;;   (interactive)
;;   (find-file (read-file-name
;;   	      "Find File on BiostatShiny: "
;;   	      "/ssh:savey@biostatshiny.jnj.com:~")))
;;   :config
;;   (setq password-cache-expiry 28800)
;;   ;; Set Bash shell to /bin/bash since only log into Linux machines
;;   (setq explicit-shell-file-name "/bin/bash") 
;;   :bind (("C-x C-g" . spa/find-file-biostat)))

