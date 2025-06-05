;;; early-init.el early-init emacs file -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Błażej Niewiadomski
;; Author: Błażej Niewiadomski <blaz.nie@protonmail.com>
;; This file is not part of GNU Emacs.
;;; Commentary:
;;
;;
;;; Code:
;; saving my eyes
(setq command-line-args (append command-line-args '("-bg" "#121212" "-fg" "#f1f1f1" "--mouse-color" "#f1f1f1"))
      ;; We're using straight.el instead of package.el, no need to load it
      package-enable-at-startup nil
      use-package-compute-statistics nil
      use-package-enable-imenu-support t
      ;; dont bother with disabled commands
      disabled-command-function nil
      straight-use-package-version 'straight
      straight-check-for-modifications '(find-when-checking check-on-save)
      read-process-output-max (* 500 1000)
      process-adaptive-read-buffering t
      ;; a bit of performance optimization on startup
      frame-inhibit-implied-resize t
      inhibit-x-resources nil
      initial-buffer-choice nil
      inhibit-startup-buffer-menu nil
      inhibit-startup-screen t
      inhibit-splash-screen t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      inhibit-redisplay nil
      ;; disable gc, we are garbage collecting on idle
      gc-cons-threshold most-positive-fixnum
      use-file-dialog nil
      use-dialog-box nil
      scroll-bar-mode nil
      use-system-tooltips nil
      menu-bar-mode nil
      tool-bar-mode nil)

(defalias 'yes-or-no-p 'y-or-n-p
  "Always use short user confirmation prompts.")

(defun +init-reset-inhibit-redisplay ()
  (setq inhibit-redisplay nil))

;; reset redisplay inhibition after init
(add-hook 'after-init-hook '+init-reset-inhibit-redisplay -100)

(setq mode-line-format nil)
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
;; to increase lsp-mode performance
(setenv "LSP_USE_PLISTS" "true")
(setq +base/font-family (pcase system-type
                          ('gnu/linux "Iosevka Nerd Font Mono")
                          ('windows-nt "Cas"))
      +base/font-weight 'semi-light
      +base/font-size (pcase system-type
                        ('windows-nt 10)
                        (_ 17))
      +base/font-spec (font-spec :family +base/font-family
                                 :weight +base/font-weight
                                 :size +base/font-size)
      +base/theme 'doom-gruber-darker)


;; Don't flicker GUI elements on startup
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(horizontal-scroll-bars . nil) default-frame-alist)
(push `(font . ,(string-join `(,+base/font-family
                               ,(number-to-string +base/font-size))
                             "-"))
      default-frame-alist)

(push '(menu-bar-lines . 0) initial-frame-alist)
(push '(tool-bar-lines . 0) initial-frame-alist)
(push '(vertical-scroll-bars . nil) initial-frame-alist)
(push '(horizontal-scroll-bars . nil) initial-frame-alist)
(push `(font . ,(string-join `(,+base/font-family
                               ,(number-to-string +base/font-size))
                             "-"))
      initial-frame-alist)

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000000))

(add-hook 'minibuffer-exit-hook 'my-minibuffer-exit-hook)
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;; run garbage collection on idle
(run-with-idle-timer 1.0 t 'garbage-collect)

(defun +set-global-font-size ()
  (set-frame-font (font-spec :family +base/font-family
                             :weight +base/font-weight
                             :size +base/font-size)
                  nil t t))

;;; early-init.el ends here
