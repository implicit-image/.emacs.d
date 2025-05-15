;;; -*- lexical-binding: t -*-

(use-package emacs
  :demand
  :custom

  ;;;; built-in global options.
  (user-full-name "Błażej Niewiadomski")
  (user-mail-address "blaz.nie@protonmail.com") ;; raised to allow better lsp speeds)
  (read-process-output-max (* 1024 16)) ;;startup screen)
  (inhibit-startup-screen t)
  (visible-bell nil)
  (ring-bell-function 'ignore)
  (display-line-numbers-type 'relative)
  (truncate-lines t)
  (truncate-partial-width-windows t)
  (create-lockfiles nil)
  (backup-inhibited t)
  (make-backup-files nil)
  (backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
  (enable-recursive-minibuffers t)
  (scroll-step 1)
  (scroll-margin 15)
  (find-file-wildcards nil)
  (comment-multi-line t)
  (comment-empty-lines t)
  (lazy-highlight-initial-delay 0)
  (completion-ignore-case t)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-short-answers t)
  (read-answer-short t)
  (left-fringe-width 5)
  (right-fringe-width 5)
  (indicate-buffer-boundaries nil)
  (indicate-empty-lines nil)
  (word-wrap t)
  (indent-tabs-mode nil)
  (tab-width 4)
  :init
  (defun +insert-scratch-buffer-info ()
    (interactive)
    (with-current-buffer (get-buffer-create "*scratch*")
      (insert (format ";;
;;                ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;                ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;                █████╗  ██╔████╔██║███████║██║     ███████╗
;;                ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;                ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;                ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;
;;   Loading time : %s
;;   Packages     : %s
;;
"
                      (emacs-init-time)
                      (number-to-string (hash-table-count straight--recipe-cache)))))

    (message (emacs-init-time))
    (lisp-interaction-mode))

  (defun +set-global-font-size ()
    (interactive)
    (let* ((weight +base/font-weight)
           (family +base/font-family)
           (size (read-number "Font size: "))
           (spec (font-spec :family family
                            :weight weight
                            :size size)))
      (set-frame-font spec nil t t)))
  ;;;; Custom global options
  (setq +base/font-family "Comic Code Ligatures"
        +base/font-weight 'semi-light
        +base/font-size (pcase system-type
                          ('windows-nt 10)
                          (t 17))
        +base/font-spec (font-spec :family +base/font-family
                                   :weight +base/font-weight
                                   :size +base/font-size)
        +base/theme 'doom-gruber-darker)

  ;;;; set default font.
  ;;;; setting `default-frame-alist' entry makes sure that emacsclient loads the correct font
  (add-to-list 'default-frame-alist `(font . ,(string-join `(,+base/font-family ,(number-to-string +base/font-size)) "-")))
  :config
  ;; always use short user input prompts
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; remove toolbar and menu bar
  (tool-bar-mode 0)
  (menu-bar-mode 0)
  ;; remove scroll bar
  (toggle-scroll-bar -1)
  ;; insert matching parens
  (electric-pair-mode 1)
  ;; highlight current line
  (global-hl-line-mode 1)
  ;; save window layout changes
  (winner-mode 1)
  ;; dont auto-revert buffers
  (global-auto-revert-mode -1)
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
                      (visual-wrap-prefix-mode 1)))
  ;; display line numbers in text-editing modes
  ((prog-mode-hook
    markdown-ts-mode-hook
    org-mode-hook
    latex-mode-hook
    org-roam-mode-hook
    gfm-mode-hook
    text-mode-hook
    conf-mode-hook
    tuareg-mode-hook)
   . display-line-numbers-mode)
  (window-setup-hook . toggle-frame-fullscreen)
  (after-init-hook . +insert-scratch-buffer-info))

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

(provide 'init-base)
