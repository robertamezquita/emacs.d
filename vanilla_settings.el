;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spa_vanilla_settings.el                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Aesthetics
;; (set-face-attribute 'default nil
;; 		    :inherit nil :stipple nil
;; 		    :inverse-video nil :box nil
;; 		    :strike-through nil :overline nil :underline nil
;; 		    :slant 'normal :weight 'normal
;; 		    :height 120 :width 'normal
;; 		    :foundry "nil" :family "Hack")
;; (set-face-attribute 'secondary-selection nil :background "PaleTurquoise2")

;; set custom theme that doesn't override background transparency
;; (load-theme 'sanityinc-solarized-dark' t)  ;; the t is a "no confirm" flag
;; (load-theme 'tsdh-dark' t)  ;; the t is a "no confirm" flag
;; (set-frame-parameter (selected-frame) 'alpha '(93 93)) ;; (focus no-focus-alpha)
;; (add-to-list 'default-frame-alist '(alpha . (93 93)))
;; (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
;; (load-theme 'nord t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'smart-mode-line-respectful' t)  ;; the t is a "no confirm" flag
(load-theme 'dracula t)



;; Emacs backup files
(setq make-backup-files nil)

;; use the default shell for programs
(setq explicit-shell-file-name "bash")
(setq shell-file-name "bash")

;; Default yes and no settings
(defalias 'yes-or-no-p 'y-or-n-p)

;; Use the value of $PATH constructed at shell's creation
;; (done only when emacs started with window-system)
;; so that ess knows where R is
(defun set-exec-path-from-shell-PATH ()
    (let ((path-from-shell (replace-regexp-in-string
                                "[ \t\n]*$"
                                ""
                (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
                (setenv "PATH" path-from-shell)
                (setq eshell-path-env path-from-shell) ; for eshell users
                (setq exec-path (split-string path-from-shell path-separator))
        )
)
(when window-system (set-exec-path-from-shell-PATH))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Built-in Modes                                                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show matching parentheses
(show-paren-mode 1)

;; overwrite selected text
(delete-selection-mode t)

;; column-number-mode
;; show row and column in status bar
(column-number-mode 1)

(use-package ls-lisp
  :init
  (setq ls-lisp-use-insert-directory-program nil))

;; Add another prompt format to prevent entering passwords in plain text
(use-package comint
  :config
  (setq comint-password-prompt-regexp
	(concat comint-password-prompt-regexp
		"\\|^Password for .*:\\s *\\'")))

;; expand abbreviations as needed
(use-package abbrev
  :defer 5
  :diminish
  :config
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
  (setq default-abbrev-mode t))

;; highlight FIXME/TODO/BUG in different font
(use-package prog-mode
  :after (auto-complete)
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (font-lock-add-keywords nil
                                      '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))

  (add-hook 'prog-mode-hook
            '(lambda ()
               (local-set-key (kbd "C-c C-u") 'string-inflection-cycle))))

;; auto-revert reads disk version periodically to refresh contents of file  
(use-package auto-revert
  :no-require t
  :diminish
  :hook ((dired-mode doc-view-mode) . auto-revert-mode)
  :config
  (setq auto-revert-verbose nil))

;; Allows for undo/redo of changes to window configs
(use-package winner
  :unless noninteractive
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

;; ediff for comparing different (versions of) files
(use-package ediff
  :config
  (setq ediff-split-window-function 'split-window-horizontally)
  :bind (("C-c C-v" . ediff-revision)))

;; for spelling corrections and writing to dictionary
(use-package ispell
  :if (not (bound-and-true-p disable-pkg-ispell))
  :defer 15
  :config
  (progn
    (cond
     ((executable-find "aspell")
      (setq ispell-program-name "aspell")
      (setq ispell-extra-args   '("--sug-mode=ultra"
                                  "--lang=en_US")))
     ((executable-find "hunspell")
      (setq ispell-program-name "hunspell")
      (setq ispell-extra-args   '("-d en_US"))))

    ;; Save a new word to personal dictionary without asking
    (setq ispell-silently-savep t))
  :bind
  ;; Similar binding to flyspell's C-. to make it easy to remember
  ;; even though this overwrites a global binding
  ("C-M-." . ispell-word))

;; (use-package flyspell
;;   :diminish (flyspell-mode . "φ")
;;   :after auto-complete
;;   :preface
;;   ;; Flyspell signals an error if there is no spell-checking tool is
;;   ;; installed. We can advice `turn-on-flyspell' and `flyspell-prog-mode'
;;   ;; to try to enable flyspell only if a spell-checking tool is available.
;;   (defun modi/ispell-not-avail-p (&rest args)
;;     "Return `nil' if `ispell-program-name' is available; `t' otherwise."
;;     (not (executable-find ispell-program-name)))  
;;   :hook
;;   ((text-mode org-mode) . turn-on-flyspell)
;;   ((prog-mode) . flyspell-prog-mode)
;;   :config
;;   (ac-flyspell-workaround)
;;   (advice-add 'turn-on-flyspell   :before-until #'modi/ispell-not-avail-p)
;;   (advice-add 'flyspell-prog-mode :before-until #'modi/ispell-not-avail-p))

;; use prettier symbols for greek letters and math
;; (use-package prettify-symbols-mode
;;   ;; Can modify `ess-r-prettify-symbols` to add additional symbols
;;   :hook
;;   ((prog-mode ess-mode inferior-ess-mode) . prettify-symbols-mode))

;; calendar
(use-package calendar
  :init
  (setq calendar-latitude 47.627546)
  (setq calendar-longitude -122.331602)
  (setq calendar-location-name "Seattle, WA")
  (setq calendar-time-zone -800)
  (setq calendar-standard-time-zone-name "PST")
  (setq calendar-daylight-time-zone-name "PDT"))

;; autocompletion package
(use-package auto-complete
  :defer 15
  :diminish
  :preface
  (defun my-auto-hook ()
    (auto-complete-mode 1)
    (define-key ac-completing-map [return] nil)
    (define-key ac-completing-map "\r" nil))
  :hook
  ((prog-mode ess-mode inferior-ess-mode) . (my-auto-hook)))

;; TODO: This isn't getting turned on in ESS modes
(use-package subword-mode
  :diminish
  :hook
  ((ess-mode-hook inferior-ess-mode-hook) . subword-mode))

;; interactive buffer magic
(use-package ibuffer
  :ensure t
  :init
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups
	'(("work"
	   ("emacs-config" (or (filename . ".emacs.d")
			       (filename . ".emacs")))
           ("R Code" (or (name . "\\.R$")
			 (name . "\\.Rmd$")))
           ("Python Code" (name . "\\.py$"))
           ("Julia Code" (name . "\\.jl$"))
	   ("Process" (or (mode . inferior-ess-mode)
			  (mode . inferior-python-mode)
			  (mode . shell-mode)
			  (mode . term-mode)))
	   ("Org" (mode . org-mode))
           ("Dired" (mode . dired-mode))
	   ("Magit" (name . "\*magit"))
	   ("Help" (or (name . "\*Help\*")
		       (name . "\*Apropos\*")
		       (name . "\*info\*"))))))
  :bind (:map global-map
	      ("C-x C-b" . ibuffer)))

  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skip splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; Turn off tool bar mode
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; disable scroll bars in GUI and menu in terminal
;;(if window-system
(scroll-bar-mode -1)
;;  (progn
(tool-bar-mode -1)
(menu-bar-mode -1)
;;    )
;;  )

;; Sort results from apropos by relevance
(setq apropos-sort-by-scores t)

;; Don't warn about opening large files! I know they are large, yes I want you
;; to open them, that is why I asked you to open it.
(setq large-file-warning-threshold nil)

;; use hippie-expand instead of dabbrev
(global-set-key [remap dabbrev-expand] 'hippie-expand)

;; Set the fill column to be 80 by default
(setq-default fill-column 80)

;; Better wild cards in search
(setq search-whitespace-regexp ".*?")

;; Set these so that `shell-command` and `compile` understand aliases in .bashrc
(setq shell-file-name "bash")
(setq shell-command-switch "-ic")

;; Location Settings
(setq display-time-world-list (quote
			       (("America/Los_Angeles" "La Jolla, US")
				("America/New_York" "Spring House, US")
				("Europe/London" "London, UK")
				("Europe/Brussels" "Beerse, BE")
				("Asia/Shanghai" "Shanghai, CN")
				("Asia/Tokyo" "Tokyo, JP"))))

;; Add the system clipboard to the Emacs kill-ring
;; (setq save-interprogram-paste-before-kill t)

;; compilation
(setq compilation-scroll-output 'first-error)

(setq trash-directory "~/.Trash")

(setq sentence-end-double-space nil)
