;;;;;;;;;;;;;;;;;
;; Programming ;;
;;;;;;;;;;;;;;;;;

;; ;; emacs ipython notebook
;; (use-package ein
;;   :ensure t)

;; setup for the ESS
(use-package ess
  :ensure t
  :init (require 'ess-site)
  :preface
  (defun spa/ess-insert-pipe (arg)
    "Insert pipe '%>%' operator, add a newline and indent. With prefix arg, don't add newline" 
    (interactive "P")
    (if arg
	(progn
	  (just-one-space)
	  (insert "%>%")
	  )
      (progn
	(just-one-space)
	(insert "%>%")
	(ess-newline-and-indent))))
  (defun spa/ess-insert-assign ()
    "Insert asignment operator for R"
    (interactive)
    (just-one-space)
    (insert "<-")
    (just-one-space))
  ;; TODO: Improve this by determining source of error messages
  ;; and automatically linking scratch R buffer to process
  (defun spa/R-scratch ()
    "Bring up a 'scratch' R script and console for quick calculations."
    (interactive)
    (delete-other-windows)
    (setq new-buf (get-buffer-create "scratch.R"))
    (switch-to-buffer new-buf)
    (R-mode)
    (setq w1 (selected-window))
    (setq w1name (buffer-name))
    (setq w2 (split-window w1 nil t))
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
	(R))
    (set-window-buffer w2 "*R*")
    (set-window-buffer w1 w1name))
  :config
  ;; prevent hanging when eval a region
  (setq ess-eval-visibly-p nil)
  (setq ess-smart-S-assign-key nil)
  ;; Don't ask me for a directory on startup
  (setq ess-ask-for-ess-directory nil)
  ;; ;; Use ido mode for ESS
  ;; (setq ess-use-ido t)
  ;; Would like to use flymake but it's not currently working
  (setq ess-use-flymake nil)
  ;; (setq ess-help-own-frame t)
  (setq ess-ask-about-transfile nil)
  ;; (setq inferior-ess-own-frame t) ; for 'dedicated' *R* buffers
  (setq inferior-R-args "--no-save") ; default is don't ask to save workspace
  (setq-default ess-dialect "R")
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)
  ;; ElDoc
  ;; This seemed to slow things down too much so disabled
  (setq ess-eldoc-show-on-symbol nil) ; shows function arguments even if not in ()
  (setq ess-plain-first-buffername nil)
  (setq ess-tab-complete-in-script t)
  ;; Modify the indentation style so that continued statements
  ;; like piping and adding operations only indent the first line
  ;; (add-to-list 'ess-style-alist
  ;;              '(my-style
  ;;                (ess-indent-level . 2)
  ;;                (ess-first-continued-statement-offset . 2)
  ;;                (ess-continued-statement-offset . 0)
  ;;                (ess-brace-offset . -4)
  ;;                (ess-expression-offset . 4)
  ;;                (ess-else-offset . 0)
  ;;                (ess-close-brace-offset . 0)
  ;;                (ess-brace-imaginary-offset . 0)
  ;;                (ess-continued-brace-offset . 0)
  ;;                (ess-arg-function-offset . 4)
  ;;                (ess-arg-function-offset-new-line . '(4))
  ;;                ))
  ;; (setq ess-default-style 'my-style)
  ;; Agressive indentations in ESS mode
  ;; (add-hook 'ess-mode-hook #'aggressive-indent-mode)
  ;; ;; For not substituting '_' with '->'
;;; ESS Roxygen customization to place nice with my function templates
  (setq ess-roxy-template-alist
	(list '("param" . "")
	      '("return" . "")))
  ;; Tried to fix e-mail address because @ gets parsed incorrectly
  ;; and it should be '@@' but too much work and it didn't work
  ;; (defun spa/fix-mail-address ()
  ;;   (save-excursion
  ;;     (while (search-backward "savey@its.jnj.com" nil t)
  ;;       (replace-match "savey@@its.jnj.com"))))
  ;; (add-hook 'ess-roxy-update-entry 'spa/fix-mail-address)
  ;; Set the font lock keywords to maximize font locking in ESS
  (setq ess-R-font-lock-keywords
	(quote
	 ((ess-R-fl-keyword:modifiers . t)
	  (ess-R-fl-keyword:fun-defs . t)
	  (ess-R-fl-keyword:keywords . t)
	  (ess-R-fl-keyword:bare-keywords . t)
	  (ess-R-fl-keyword:control-flow-keywords . t)
	  (ess-R-fl-keyword:signal-keywords . t)
	  (ess-R-fl-keyword:assign-ops . t)
	  (ess-R-fl-keyword:constants . t)
	  (ess-fl-keyword:fun-calls)
	  (ess-fl-keyword:numbers . t)
	  (ess-fl-keyword:operators . t)
	  (ess-fl-keyword:delimiters . t)
	  (ess-fl-keyword:= . t)
	  (ess-R-fl-keyword:F&T . t))))
  ;; Toggle camel case / underscore / etc.
  (add-hook 'inferior-ess-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-u") 'string-inflection-cycle)))
  (add-hook 'ess-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-u") 'string-inflection-cycle)))
  ;; Unbind ess-display-help-on-object from C-c C-v since it's on multiple keys
  (add-hook 'ess-mode-hook
            '(lambda ()
               (local-unset-key (kbd "C-c C-v"))))
  ;; Get proper R program depending on local vs. server
  (add-hook 'ess-mode-hook
            '(lambda ()
	       ;; returns non-nil if buffer is under tramp connection (remote)
	       (if (file-remote-p default-directory)
		   (setq inferior-ess-r-program "/usr/bin/R")
		 (setq inferior-ess-r-program "/usr/local/bin/R"))))
  (add-hook 'inferior-ess-mode-hook
            '(lambda ()
	       ;; returns non-nil if buffer is under tramp connection (remote)
	       (if (file-remote-p default-directory)
		   (setq inferior-ess-r-program "/usr/bin/R")
		 (setq inferior-ess-r-program "/usr/local/bin/R"))))
  :bind (:map ess-mode-map
	;;      ("<backtab>" . ess-complete-object-name)
	      ("<backtab>" . ess-r-complete-object-name)
	      ("C-c M-c" . ess-eval-paragraph-and-go)
	      ("M-=" . spa/ess-insert-pipe)
	      ("M--" . spa/ess-insert-assign))
  :bind (:map inferior-ess-mode-map
	      ("<backtab>" . ess-r-complete-object-name)
	      ("M-r" . comint-history-isearch-backward)
	      ("C-u M-r" . comint-history-isearch-backward-regexp)
	      ("M-=" . spa/ess-insert-pipe)
  	      ("M--" . spa/ess-insert-assign))
  :bind (:map global-map
	      ("C-x 9" . spa/R-scratch)))

;; (use-package ess-edit)
;; (load (xah-get-fullpath "lisp/ess-R-object-popup"))
;; Issue 8 version
;; (load (xah-get-fullpath "lisp/ess-view/ess-view.el"))
;; (setq ess-view--spreadsheet-program "open")


(use-package markdown-mode
  :ensure t
  :mode
  (("\\`README\\.md\\'" . gfm-mode)
   ("\\`readme\\.md\\'" . gfm-mode)
   ("\\.md\\'"          . markdown-mode)
   ("\\.markdown\\'"    . markdown-mode))
  :init
  (setq markdown-command "pandoc -c ~/.emacs.d/github-pandoc.css --from gfm --to html5+smart --mathjax --variable 'mathjax-url:https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML' --highlight-style pygments --standalone --self-contained --quiet"))

(use-package pandoc-mode
  :ensure t
  :hook ((markdown-mode) . pandoc-mode))

(use-package poly-markdown
  :ensure t)

(use-package poly-R
  :ensure t)

(use-package poly-noweb
  :ensure t)

(use-package polymode
  :ensure t
  :after (poly-markdown poly-R poly-noweb)
  :init
  (require 'polymode-core)
  :mode
  ("\\.Rmd" . poly-markdown+r-mode)
  :preface
  (defvar pm/chunkmode)
  (defvar rmd-render-history nil "History list for spa/rmd-render.")
  (declare-function pm-map-over-spans "polymode-core")
  (declare-function pm-narrow-to-span "polymode-core")
  (defun spa/rmd-render (arg)
    "Render the current Rmd file to first output format in YAML header.
With a prefix arg, edit the R command in the minibuffer"
    (interactive "P")
    ;; Find the first output type and use that
    (save-excursion
      (beginning-of-buffer)
      (search-forward-regexp "^output[:]")
      (next-line)
      (setq output-format (thing-at-point 'line t))
      (setq output-format (s-replace ":" "" output-format))
      (setq output-format (replace-regexp-in-string
			   (rx (or (: bos (* (any " \t\n")))
                                   (: (* (any " \t\n")) eos)))
                           ""
                           output-format)))
    ;; Build the default R render command
    (setq rcmd (concat "rmarkdown::render('" buffer-file-name "',"
		       "output_dir = '../reports',"
		       "output_format = '" output-format "')"))
    ;; Check for prefix argument
    (if arg
	(progn
	  ;; Use last command as the default (if non-nil) 
	  (setq prev-history (car rmd-render-history))
	  (if prev-history
	      (setq rcmd prev-history)
	    nil)
	  ;; Allow the user to modify rcmd 
	  (setq rcmd
		(read-from-minibuffer "Run: " rcmd nil nil 'rmd-render-history))
	  )
      ;; With no prefix arg, add default rcmd to history
      (setq rmd-render-history (add-to-history 'rmd-render-history rcmd)))
    ;; Build and evaluate the shell command
    (setq command (concat "echo \"" rcmd "\" | R --vanilla"))
    (compile command))
  ;; TODO: Debug this for edge cases. Usually this works ok
  (defun spa/rmd-run ()
    "Start a Shiny server for the given R markdown document, 
   and render it for display."
    (interactive)
    (save-excursion
      (setq orig-buffer (buffer-name))
      ;; Build the R run command
      (setq rcmd (concat "rmarkdown::run('" buffer-file-name "')"))
      ;; Change buffer name to [r] buffer associated with ESS process
      (if (string-match-p "\[r\]" orig-buffer)
	  (setq r-buf-name orig-buffer)
	(setq r-buf-name (concat orig-buffer "[r]")))
      (switch-to-buffer r-buf-name)
      ;; Send the R command to the R process
      (setq process (ess-get-process))
      ;; Interrupt current R process
      (switch-to-buffer (process-buffer process))
      (interrupt-process)
      (switch-to-buffer orig-buffer)
      (ess-send-string process rcmd t)))
  (defun spa/rmd-send-chunk ()
    "Send current R chunk to ess process."
    (interactive)
    (and (eq (oref pm/chunkmode :mode) 'r-mode) ;;'
	 (pm-with-narrowed-to-span nil
           (goto-char (point-min))
           (forward-line)
           (ess-eval-region (point) (point-max) nil nil 'R)))) ;;'
  (defun spa/rmd-send-chunk-and-step ()
    "Send current R chunk to ess process and advance to next chunk."
    (interactive)
    (and (eq (oref pm/chunkmode :mode) 'r-mode) ;;'
	 (ess-eval-buffer 'R)
	 (search-forward "```")
	 (search-forward "```")
	 (forward-line)))
  ;; TODO: Debug this
  (defun spa/rmd-send-buffer (arg)
    "Send all R code blocks in buffer to ess process. With prefix
Send regions above point."
    (interactive "P")
    (save-restriction
      (widen)
      (save-excursion
	(pm-map-over-spans
	 'spa/rmd-send-chunk (point-min) ;;'
	 ;; adjust this point to send prior regions
	 (if arg (point) (point-max))))))
  (defun spa/insert-r-code-chunk (arg)
    "Insert R Markdown code chunk. With prefix arg, read in chunk header contents"
    (interactive "P")
    (if arg
	(progn
	  (setq contents (read-from-minibuffer "Chunk Header: " nil nil nil))
	  (setq str (concatenate 'string  "```{" contents "}\n"))
	  (insert str)
	  )
      (progn
	(insert "```{r}\n")))
    (insert "\n")
    (save-excursion
      (insert "\n")
      (insert "\n")
      (insert "```\n")))
  :config
  (define-key poly-markdown+R-mode-map (kbd "C-c C-f")  'spa/rmd-send-chunk-and-step)
  (define-key poly-markdown+R-mode-map (kbd "C-c C-b")  'spa/rmd-send-buffer)
  (define-key markdown-mode-map (kbd "C-c C-a c") 'spa/insert-r-code-chunk)
  :bind (:map polymode-minor-mode-map
	      ("C-c r" . spa/rmd-render)
	      ("C-c s" . spa/rmd-run)))


(use-package elpy
  :ensure t
  :init
  (setq elpy-shell-use-project-root nil)
  :config
  (elpy-enable)
  (add-hook 'elpy-mode-hook (lambda () (elpy-shell-set-local-shell
				   (elpy-project-root))))
  ;;  Switch to flycheck instead of flymake for real-time syntax checking
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  ;; Set the default shell interpreter to Jupyter for interactive Python
  (setq python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console --simple-prompt"
	python-shell-prompt-detect-failure-warning nil)
  (add-to-list 'python-shell-completion-native-disabled-interpreters
               "jupyter"))
  ;; ;; enable autopep8 formatting on save
  ;; (require 'py-autopep8)
  ;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


(use-package yaml-mode
  :ensure t
  :defer t
  :mode
  ("\\.yml\\'" . yaml-mode))
