;;; -*- lexical-binding: t -*-

(use-package lispy
  :commands
  lispy-mode
  :init
  (defun +lisp/slurp-or-barf-left (&optional times)
    ""
    (interactive "p")
    (lispy-left 1)
    (lispy-slurp-or-barf-left times))
  (defun +lisp/slurp-or-barf-right (&optional times)
    ""
    (interactive "p")
    (lispy-right 1)
    (lispy-slurp-or-barf-right times))
  :hook
  ((emacs-lisp-mode common-lisp-mode scheme-mode racket-mode dune-mode) . lispy-mode)
  :general
  (lispy-mode-map
   :states '(insert normal visual)
   :prefix "C-c"
   ">" '+lisp/slurp-or-barf-left
   "<" '+lisp/slurp-or-barf-right))

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
  (+windows-cfg '((sly-repl-mode)
                  :position bottom :height 0.3 :noselect nil :dedicated t :stick nil))
  (setq inferior-lisp-program "sbcl"))

(use-package common-lisp-snippets)

(use-package lisp-mode
  :straight nil
  :mode "\\.lisp\\'"
  :hook (common-lisp-mode . (lambda ()
                              (interactive)
                              (slime-mode +1))))

(provide 'init-lisp)
