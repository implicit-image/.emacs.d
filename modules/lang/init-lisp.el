;;; -*- lexical-binding: t -*-

(use-package lispy
  :commands
  lispy-mode
  :hook
  ((emacs-lisp-mode-hook common-lisp-mode-hook scheme-mode-hook racket-mode-hook dune-mode-hook) . lispy-mode)
  :general
  (lispy-mode-map
   :states '(insert emacs)
   "C-c g" 'lispy-ace-paren)
  (lispy-mode-map
   :states '(insert emacs normal)
   "C-c <" 'lispy-move-left
   "C-c >" 'lispy-move-right
   "C-M-[" 'lispy-wrap-brackets
   "C-M-{" 'lispy-wrap-braces
   "C-M-(" 'lispy-wrap-round))

;; emacs lisp
(use-package emacs-lisp-mode
  :straight nil
  :init
  (+lookup-set-fn 'buffer
                  '(emacs-lisp-mode . helpful-at-point)
                  '(lisp-interaction-mode . helpful-at-point)))

;; racket
(use-package racket-mode)

;; scheme
(use-package geiser)

;; common-lisp
(use-package sly
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package common-lisp-snippets)

(use-package lisp-mode
  :straight nil
  :mode "\\.lisp\\'"
  :hook (common-lisp-mode-hook . (lambda ()
                                   (interactive)
                                   (slime-mode +1))))

(provide 'init-lisp)
