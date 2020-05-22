;; Much inspiration from Stefan: https://github.com/stefanavey/dot-emacs/b

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package Repositories                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-check-signature 'nil)
(setq package-archives
      '(
	("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")))
;;	("melpa-stable" . "https://stable.melpa.org/packages/")
;;	("org" . "http://orgmode.org/elpa/")))
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


;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    (quote
;;     (groovy-mode zone-rainbow yaml-mode writegood-mode which-key volatile-highlights use-package smex smartscan smart-mode-line reveal-in-osx-finder rainbow-identifiers rainbow-delimiters rainbow-blocks poly-R pandoc-mode ox-pandoc osx-dictionary org-clock-convenience org-bullets openwith northcode-theme nordless-theme nord-theme magit hl-todo goto-chg google-this flycheck f ess emojify elpy dracula-theme dired-rainbow dired-dups dired-du diminish default-text-scale counsel-projectile color-theme-sanityinc-solarized auto-complete anzu ace-window))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (unfill zone-rainbow yaml-mode writegood-mode which-key volatile-highlights use-package smex smartscan smart-mode-line reveal-in-osx-finder rainbow-identifiers rainbow-delimiters rainbow-blocks poly-R pandoc-mode ox-pandoc osx-dictionary org-clock-convenience org-bullets openwith northcode-theme nordless-theme nord-theme magit hl-todo groovy-mode goto-chg google-this flycheck f ess emojify elpy dracula-theme dired-rainbow dired-dups dired-du diminish default-text-scale counsel-projectile color-theme-sanityinc-solarized auto-complete anzu ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
