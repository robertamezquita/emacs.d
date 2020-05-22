;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spa_packages.el                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;; Libraries ;;
;;;;;;;;;;;;;;;

(use-package f
  :ensure t
  :defer t)

(use-package s
  :ensure t
  :defer t)

(use-package cl)
(use-package color)


;; Images
(add-to-list 'exec-path "/usr/local/Cellar/ImageMagick")

;;;;;;;;;;;;;;;;
;; Aesthetics ;;
;;;;;;;;;;;;;;;;
;; Turn minor mode "default text scale" on in order to globally adjust text size
;; with C-M-= and C-M--
(use-package default-text-scale
  :ensure t
  :config (default-text-scale-mode t))

(use-package smart-mode-line
  :ensure t
  :init (add-hook 'after-init-hook 'sml/setup)
  :config
  (setq sml/theme 'respectful)
  (setq sml/modified-char "m"))

(use-package color-theme-sanityinc-solarized
  :ensure t
  :after (smart-mode-line)
  :init
  (setq custom-safe-themes t) ;; treat all themes as safe
  :config
  (color-theme-sanityinc-solarized--define-theme dark)
  (load-theme 'smart-mode-line-respectful))

(use-package emojify
  :ensure t
  :defer 15
  :hook ((text-mode org-mode) . global-emojify-mode)
  :config
  (setq emojify-program-contexts '(comments))
  :bind ("C-c E" . emojify-insert-emoji))

;; dired with recursive directory sizes
(use-package dired-du
  :ensure t
  :after dired
  :defer t
  :config
  (setq dired-du-size-format t)
  (setq dired-du-update-headers t))

;; rainbow delimiters for pretty parens/braces
(use-package rainbow-delimiters
  :ensure t
  :after (color cl)
  :preface
  (defun hsl-to-hex (h s l)
    "Convert H S L to hex colours"
    (let (rgb)
      (setq rgb (color-hsl-to-rgb h s l))
      (color-rgb-to-hex (nth 0 rgb)
			(nth 1 rgb)
			(nth 2 rgb))))
  (defun bracket-colors ()
    "Calculate the bracket colours based on background.
Used for better rainbow colors than default.
source: `https://emacs.stackexchange.com/questions/21303/looking-for-a-better-way-of-tweaking-rainbow-delimiters'"
    (let (hexcolors lightvals)
      (if (>= (color-distance  "white"
                               (face-attribute 'default :background))
              (color-distance  "black"
                               (face-attribute 'default :background)))
          (setq lightvals (list 0.65 0.55))
	(setq lightvals (list 0.35 0.30)))
      (concatenate 'list
                   (dolist (n'(.71 .3 .11 .01))
                     (push (hsl-to-hex (+ n 0.0) 1.0 (nth 0 lightvals)) hexcolors))
                   (dolist (n '(.81 .49 .17 .05))
                     (push (hsl-to-hex (+ n 0.0) 1.0 (nth 1 lightvals)) hexcolors)))
      (reverse hexcolors)))
  :config
  (set-face-attribute 'popup-face nil
		      :background "white" :foreground "black")
  (set-face-attribute `rainbow-delimiters-depth-1-face nil
  		      :foreground (nth 0 (bracket-colors)))
  (set-face-attribute `rainbow-delimiters-depth-2-face nil
  		      :foreground (nth 1 (bracket-colors)))
  (set-face-attribute `rainbow-delimiters-depth-3-face nil
  		      :foreground (nth 2 (bracket-colors)))
  (set-face-attribute `rainbow-delimiters-depth-4-face nil
  		      :foreground (nth 3 (bracket-colors)))
  (set-face-attribute `rainbow-delimiters-depth-5-face nil
  		      :foreground (nth 4 (bracket-colors)))
  (set-face-attribute `rainbow-delimiters-depth-6-face nil
  		      :foreground (nth 5 (bracket-colors)))
  (set-face-attribute `rainbow-delimiters-depth-7-face nil
  		      :foreground (nth 6 (bracket-colors)))
  (set-face-attribute `rainbow-delimiters-depth-8-face nil
  		      :foreground (nth 7 (bracket-colors)))
  (set-face-attribute `rainbow-delimiters-depth-9-face nil
  		      :foreground (nth 8 (bracket-colors)))
  (set-face-attribute `rainbow-delimiters-unmatched-face nil
  		      :foreground "white" :background "red")
  :hook ((prog-mode ess-mode) . rainbow-delimiters-mode))

;; (use-package rainbow-delimiters
;;   :ensure t
;;   :hook ((prog-mode ess-mode) . rainbow-delimiters))

;; highlight todo keywords
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1))

;; better switching of windows with keyboard!
(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind* ("M-o" . ace-window))

;; popup interface for autocomplete and such
(use-package popup
  :ensure t
  :config
  (set-face-attribute 'popup-face nil :background "white" :foreground "black")
  (set-face-attribute 'popup-tip-face nil :background "white" :foreground "black")
  (set-face-attribute 'popup-menu-selection-face nil :background "#92C1F0" :foreground "black")
  (set-face-attribute 'popup-isearch-match nil :background "#92C1F0" :foreground "black"))

;; ;; interpret csv mode better
;; (use-package csv-mode
;;   :ensure t
;;   :mode
;;   ("\\.csv\\'" . csv-mode)
;;   ("\\.tsv\\'" . csv-mode)
;;   :init
;;   (add-hook 'csv-mode-hook (lambda () (font-lock-mode -1)))
;;   (add-hook 'csv-mode-hook (lambda () (visual-line-mode -1)))
;;   (add-hook 'csv-mode-hook #'flyspell-mode-off)
;; 					; This hook makes csv-mode align the fields by default in the entire buffer
;;   (add-hook 'csv-mode-hook
;; 	    (lambda () (csv-align-fields nil (point-min) (point-max)))))

;; use google for looking stuff up
(use-package google-this
  :ensure t
  :bind
  ("C-c C-g" . google-this))

;; volatile highlights - temporarily highlight changes from pasting etc
(use-package volatile-highlights
  :ensure t
  :diminish
  :config
  (volatile-highlights-mode t))

;; use the mac dictionary
(use-package osx-dictionary
  :ensure t
  :bind
  ("C-c d" . osx-dictionary-search-word-at-point))

;; jumps between other symbols found at point
(use-package smartscan
  :ensure t
  :config
  (global-smartscan-mode 1)
  :bind (("M-<down>" . smartscan-symbol-go-forward)
	 ("M-<up>" . smartscan-symbol-go-backward)))

;; open with logical programs on computer outside emacs
(use-package openwith
  :ensure t
  :init
  :config
  (openwith-mode t)
  (setq openwith-associations
        (list
         (list (openwith-make-extension-regexp
                '("pdf" "doc" "docx" "xls" "xlsx" "ppt" "pptx"))
               "open"
               '(file))
         )))

;; view pdfs in emacs!
(use-package doc-view
  :ensure t
  :defer t
  :mode ("\\.pdf\\'" . doc-view-mode-maybe)
  :config
  (setq doc-view-continuous t))

;; helps with sorting of ivy/counsel completions
(use-package smex
  :ensure t
  :config
  (defadvice smex (around space-inserts-hyphen activate compile)
    (let ((ido-cannot-complete-command
	   `(lambda ()
	      (interactive)
	      (if (string= " " (this-command-keys))
		  (insert ?-)
		(funcall ,ido-cannot-complete-command)))))
      ad-do-it))
  :bind (("M-x" . smex)
	 ("M-X" . smex-major-mode-commands)
	 ("C-c C-c M-x" . execute-extended-command)))

;; write better, without passive voice, duplicate words, or weasel words
(use-package writegood-mode
  :ensure t)

;; pdf tools
;; (use-package pdf-tools
;;   :ensure t
;;   :config
;;   (pdf-tools-install))

;; know what keys do
(use-package which-key
  :ensure t
  :defer 5
  :diminish
  :commands which-key-mode
  :config
  (which-key-mode)
  :bind (:map global-map
	      ("C-h z" . which-key-show-major-mode)))

;; go to your last change on the fly
(use-package goto-chg
  :ensure t
  :bind
  ("C-c l" . goto-last-change))

;; use ivy for autocompletion
(use-package ivy
  :ensure t
  :diminish
  :preface
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)  
  (setq ivy-count-format "(%d/%d) ")
  (add-to-list 'ivy-ignore-buffers "\\[r\\]")
  (add-to-list 'ivy-ignore-buffers "\\[poly-head-tail\\]")
  (add-to-list 'ivy-ignore-buffers "\\[yaml\\]")
  (add-to-list 'ivy-ignore-buffers "\\[latex\\]")
  (add-to-list 'ivy-ignore-buffers "\\[fallback\\]")
  (add-to-list 'ivy-ignore-buffers "\\[sas\\]")
  (set-face-attribute 'ivy-current-match nil :background "ns_selection_bg_color" :foreground "ns_selection_fg_color")
  :bind
  ("C-x b" . ivy-switch-buffer))

;; use counsel for awesomeness
(use-package counsel
  :ensure t
  :after ivy
  :diminish
  :preface
  :config
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c g" . counsel-git)
  ("C-c r" . counsel-minibuffer-history))

;; swipe right on me
(use-package swiper
  :ensure t
  :after ivy
  :config
  (global-set-key "\C-s" 'swiper)
  :bind (:map isearch-mode-map
	      ("C-o" . swiper-from-isearch)))

;; manage your projects with projectile
(use-package projectile
  :defer 5
  :diminish
  :init
  ;; (setq projectile-project-search-path '("~/Repos/"))
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind* ("C-c P" . (lambda () (interactive)
                      (projectile-cleanup-known-projects)
                      (projectile-discover-projects-in-search-path)))
  :config
  (projectile-global-mode))

(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

;; (use-package ibuffer-projectile
;;   :ensure t
;;   :defer t
;;   :after (ibuffer projectile))

;; (use-package ibuffer-vc
;;   :ensure t
;;   :defer t
;;   :after (ibuffer))

;; jump around within a buffer super fast
(use-package avy
  :ensure t
  :config
  (avy-setup-default)
  :bind
  ("C-:" . avy-goto-char)
  ("C-'" . avy-goto-char-2)
  ("M-g f" . avy-goto-line)
  ("M-g w" . avy-goto-word-1))

;; highlights matches and works for replace
(use-package anzu
  :ensure t
  :diminish
  :config
  (global-anzu-mode)
  :bind (:map global-map
	      ("M-%" . anzu-query-replace)
	      ("C-M-%" . anzu-query-replace-regexp)))

;; better undo 
(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode))

;; check for spelling and the like
(use-package flycheck
  :ensure t
  :init
  (setq flycheck-lintr-linters "with_defaults(todo_comment_linter = NULL, trailing_blank_lines_linter = NULL)")
  :commands
  (flycheck-mode
   flycheck-next-error
   flycheck-previous-error))

;; (use-package template
;;   :load-path (lambda () (xah-get-fullpath "lisp/template/lisp/"))
;;   :config
;;   (template-initialize)
;;   ;; TODO: Try to set default directory to one in dot-emacs repos!
;;   ;; Tried many ways and it doesn't work
;;   ;; (setq template-default-directories (xah-get-fullpath "lisp/template/templates/"))
;;   :bind
;;   ("C-c i" . template-expand-template))

;; better git management
(use-package magit
  :ensure t
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch-popup))

;; open the current file in OSX finder
(use-package reveal-in-osx-finder
  :ensure t
  :bind (:map global-map
	      ("C-c o" . reveal-in-osx-finder)))


