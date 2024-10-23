(require 'f)


(use-package emacs
  :demand
  :init
  (setq +base/font-family "Comic Code Semi Bold"
	+base/font-weight 'semi-light
	+base/font-size 20
	+base/font-spec (font-spec :family +base/font-family
				   :weight +base/font-weight
				   :size +base/font-size))
  ;; (set-frame-font +base/font-spec nil t)
  (add-to-list 'default-frame-alist `(font . ,(string-join `(,+base/font-family ,(number-to-string +base/font-size)) "-")))
  (setq user-full-name "Błażej Niewiadomski"
        user-mail-address "blaz.nie@protonmail.com"
	;;startup screen
        inhibit-startup-screen t
	
        visible-bell nil
	;; lines
        display-line-numbers-type 'relative
	truncate-lines t
	truncate-partial-width-windows t
	;;tempfiles
        create-lockfiles nil
	backup-directory-alist `(("." . ,(f-join user-emacs-directory "backups/")))
	;; increase garbage collector limit
	gc-cons-threshold 100000000
	;; scrollling
	scroll-step 1
	scroll-margin 15)
  :config
  ;; always use short user input prompts
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; remove toolbar and menu bar
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  ;; line numbers
  (global-display-line-numbers-mode 1)
  ;; remove scroll bar
  (toggle-scroll-bar -1)
  ;; highlight matching parens
  (show-paren-mode 1)
  ;; insert matching parens
  (electric-pair-mode 1)
  ;; highlight current line
  (global-hl-line-mode 1)
  ;; show borders between windows
  (window-divider-mode 1)
  (add-to-list 'default-frame-alist
               '(vertical-scroll-bars . nil))
  ;; write customizations to seperate file
  (let ((customization-file
         (expand-file-name "custom.el" user-emacs-directory)))
    (unless (file-exists-p customization-file)
      (write-region "" nil customization-file))
    (setq custom-file customization-file)
    (load custom-file 'noerror))
  :hook
  (prog-mode . (lambda () (toggle-truncate-lines 1))))


(provide 'implicit-base)
