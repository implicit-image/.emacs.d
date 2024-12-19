;;; -*- lexical-binding: t -*-

(use-package emacs
  :demand
  :init
  (require 'f)
  (setq +base/font-family "Comic Code" ;; "Iosevka Comfy Fixed"
        +base/font-weight 'semi-light
        +base/font-size 17
        +base/font-spec (font-spec :family +base/font-family
                                   :weight +base/font-weight
                                   :size +base/font-size)
	+base/theme 'doom-gruber-darker)
  ;; (set-frame-font +base/font-spec nil t)
  (add-to-list 'default-frame-alist `(font . ,(string-join `(,+base/font-family ,(number-to-string +base/font-size)) "-")))
  (setq user-full-name "Błażej Niewiadomski"
	user-mail-address "blaz.nie@protonmail.com"
	read-process-output-max (* 1024 16)
	;;startup screen
	inhibit-startup-screen t
	visible-bell nil
	debug-on-error nil
	;; lines
	display-line-numbers-type 'relative
	truncate-lines t
	truncate-partial-width-windows t
	;;tempfiles
	create-lockfiles nil
	backup-directory-alist `(("." . ,(f-join user-emacs-directory "backups")))
	;; increase garbage collector limit
	gc-cons-threshold (* 1024 1024 10)
	;; scrollling
	scroll-step 1
	scroll-margin 15
	find-file-wildcards nil)
  :config
  ;; always use short user input prompts
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; remove toolbar and menu bar
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  (indent-tabs-mode -1)
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
;;;  auto revert all buffers
  (global-auto-revert-mode nil)
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
  (prog-mode . (lambda () (toggle-truncate-lines 1)))
  ((prog-mode
    markdown-mode
    org-mode
    latex-mode
    org-roam-mode
    gfm-mode
    text-mode
    conf-mode
    tuareg-mode)
   . display-line-numbers-mode))

;; load $PATH from shell
(use-package exec-path-from-shell
  :commands exec-path-from-shell-initialize
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "JAVA_HOME"))
  :hook
  (after-init . exec-path-from-shell-initialize))

(provide 'init-base)
