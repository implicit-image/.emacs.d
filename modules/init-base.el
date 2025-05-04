;;; -*- lexical-binding: t -*-

(use-package emacs
  :demand
  :init
  (require 'f)

  ;;;; Custom global options
  (setq +base/font-family "Comic Code Ligatures"
        +base/font-weight 'semi-light
        +base/font-size (pcase system-type
                          ('windows-nt 12)
                          (t 17))
        +base/font-spec (font-spec :family +base/font-family
                                   :weight +base/font-weight
                                   :size +base/font-size)
        +base/theme 'doom-gruber-darker)

  ;;;; set default font.
  ;;;; setting `default-frame-alist' entry makes sure that emacsclient loads the correct font
  (add-to-list 'default-frame-alist `(font . ,(string-join `(,+base/font-family ,(number-to-string +base/font-size)) "-")))

  ;;;; built-in global options.
  (setq user-full-name "Błażej Niewiadomski"
        user-mail-address "blaz.nie@protonmail.com"
        ;; raised to allow better lsp speeds
        read-process-output-max (* 1024 16)
        ;;startup screen
        inhibit-startup-screen t
        ;; debug-on-error t
        visible-bell nil
        ring-bell-function 'ignore
        ;; lines
        display-line-numbers-type 'relative
        truncate-lines t
        truncate-partial-width-windows t
        ;;tempfiles
        create-lockfiles nil
        ;; put backups in a shared directory
        backup-directory-alist `(("." . ,(f-join user-emacs-directory "backups")))
        ;; increase garbage collector limit
        ;; this increases the time emacs can run w/o running garbage collector,
        ;; but also increases time the garbage collection takes when it is needed
        ;; 10 MB is a nice in-between value
        gc-cons-threshold (* 1024 1024 10)
        ;; enable recursive minibuffers for vertico
        enable-recursive-minibuffers t
        ;; scrollling
        scroll-step 1
        scroll-margin 15
        ;; TODO: find useful wildard settings
        find-file-wildcards nil)
  :config
  ;; always use short user input prompts
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; remove toolbar and menu bar
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  ;; remove scroll bar
  (toggle-scroll-bar -1)
  ;; highlight matching parens
  (show-paren-mode 1)
  ;; insert matching parens
  (electric-pair-mode 1)
  ;; highlight current line
  (global-hl-line-mode 1)
  ;; show borders between emacs windows
  (window-divider-mode 1)
  ;; dont auto-revert buffers
  (global-auto-revert-mode nil)
  ;; dont show scroll-bars
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
  ;; truncate lines in modes derived from prog-mode
  ;; better to see all of the code
  (prog-mode-hook . (lambda ()
                      (interactive)
                      (toggle-truncate-lines 1)
                      (visual-wrap-prefix-mode 1)))
  ;; display line numbers in text-editing modes
  ((prog-mode-hook
    markdown-mode-hook
    org-mode-hook
    latex-mode-hook
    org-roam-mode-hook
    gfm-mode-hook
    text-mode-hook
    conf-mode-hook
    tuareg-mode-hook)
   . display-line-numbers-mode)
  (window-setup-hook . toggle-frame-fullscreen))

;; load $PATH from shell
(use-package exec-path-from-shell
  :commands exec-path-from-shell-initialize
  :if (+os/is-linux-p)
  :init
  (setq exec-path-from-shell-variables '("PATH" "TERM" "MANPATH" "JAVA_HOME")
        exec-path-from-shell-shell-name (cond ((or (+os/is-wsl-p)
                                                   (+os/is-linux-p))
                                               "zsh")
                                              ((+os/is-windows-p) "powershell")))
  :hook
  (after-init . exec-path-from-shell-initialize))

(use-package mule
  :straight nil
  :general
  (+leader-keys
    "t I" '("Select input method" . set-input-method)))

(use-package restart-emacs
  :general
  (+leader-keys
    "q r" '("Restart emacs" . restart-emacs)))

(provide 'init-base)
