;; Much inspiration from Stefan: https://github.com/stefanavey/dot-emacs/b

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Repositories                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives
      '(
	("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;;;;;;;;;;;;;;;;;
;; use-package ;;
;;;;;;;;;;;;;;;;;
(package-refresh-contents)
(dolist (package '(use-package diminish bind-key))
   (unless (package-installed-p package)
     (package-install package)))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant


;; Custom function to get full path relative to caller's location
(defun xah-get-fullpath (@file-relative-path)
"
  Return the full path of *file-relative-path, relative to caller's file location.
  Example: If you have this line `(xah-get-fullpath \"../xyz.el\")` in the 
  file at `/home/mary/emacs/emacs_lib.el`, then the return value is `/home/mary/xyz.el`
  See `https://github.com/stefanavey/dot-emacs/blob/master/spa_emacs_init.el` for more details.
"

  (concat (file-name-directory (or load-file-name buffer-file-name)) @file-relative-path)
)

(defconst dot-emacs-path (xah-get-fullpath "."))


;; Load add-on packages
(load (xah-get-fullpath "packages"))

;; Load plain Emacs settings
(load (xah-get-fullpath "vanilla_settings"))

;; Load programming related packages
(load (xah-get-fullpath "programming"))

;; Load org-mode (gtd) related packages
(load (xah-get-fullpath "org_settings"))



;; ;; Custom set variables by emacs --------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dired-dups zone-rainbow dired-rainbow rainbow-blocks rainbow-identifiers auto-complete org-bullets ox-pandoc org-clock-convenience yaml-mode elpy poly-R poly-markdown pandoc-mode markdown-mode ess reveal-in-osx-finder magit flycheck undo-tree anzu counsel-projectile counsel ivy goto-chg which-key writegood-mode smex openwith smartscan osx-dictionary volatile-highlights google-this popup ace-window hl-todo rainbow-delimiters dired-du use-package smart-mode-line f emojify diminish default-text-scale color-theme-sanityinc-solarized))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
