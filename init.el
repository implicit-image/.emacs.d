;;; -*- lexical-binding: t -*-
;; set up straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(straight-use-package 'use-package)

;; make use-package use straight.el by default
(setq straight-use-package-by-default t
      use-package-hook-name-suffix nil
      ;; lazy load by default
      use-package-always-defer t)

;; early org mode declaration to make sure the correct version gets loaded
(use-package org
  :straight `(org
              :fork (:host nil
                           :repo "https://git.tecosaur.net/tec/org-mode.git"
                           :branch "dev"
                           :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
                (require 'lisp-mnt)
                (let ((version
                       (with-temp-buffer
                         (insert-file-contents "lisp/org.el")
                         (lm-header "version")))
                      (git-version
                       (string-trim
                        (with-temp-buffer
                          (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                          (buffer-string)))))
                  (insert
                   (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                   (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                   "(provide 'org-version)\n")))
              :pin nil))


(require 'init-modules (format "%smodules/init-modules.el" user-emacs-directory))
(require 'init-os)
(require 'init-elisp-libs)
(require 'init-keybindings)
(require 'init-base)

;; keybindings
(require 'init-meow)
;; (require 'init-evil)

(require 'init-utils)
(require 'init-search)

;;;; appearance
(require 'init-appearance)
(require 'init-windows)
(require 'init-modeline)

;;; minibuffer completion
(require 'init-completion)

;;;; programming utilities
(require 'init-lookup)
(require 'init-format)
(require 'init-edit)
;;;; validating code and showing errors
(require 'init-checkers)
;;;; inserting and managing snippets
(require 'init-snippets)
(require 'init-indent)
;;;; debugging code
(require 'init-debug)
;;;; compiling and running code
(require 'init-compile)
;;;; custom LSP integration and utils
;; (require 'init-lsp)

;;;; standard elisp lsp client
;; (require 'init-lsp-mode)
(require 'init-cape)
(require 'init-corfu)
;; (require 'init-eglot)
;; (require 'init-lsp-bridge)
(require 'init-lsp-mode)

(require 'init-ui)
(require 'init-languages)
(require 'init-projects)

;;;; version control
(require 'init-vc)

;;;; terminal
(require 'init-terminal)

;;;; browsing
(require 'init-files)
(require 'init-buffers)
(require 'init-treemacs)
(require 'init-dired)
(require 'init-remote)

;;;; org mode
(require 'init-org)


;;;; tty emacs options
(require 'init-tty)

;;;; books
(require 'init-books)

;;;; llm integration
(require 'init-llm)

;;; social media clients
(require 'init-socials)
(require 'init-media)
;;; start server
(require 'server)
(when (not (server-running-p))
  (server-start))

;; reset gc limit after init file
;; (setq gc-cons-threshold (* 1024 1024 10))
