(use-package lispy
  :commands
  lispy-mode
  :init
  (add-to-list '+edit/evil-mc-incompatible-modes 'lispy-mode)
  :hook ((emacs-lisp-mode common-lisp-mode) . lispy-mode))

;; emacs lisp
(use-package emacs-lisp-mode
  :straight nil
  :init
  (+lookup-set-fn 'buffer '((emacs-lisp-mode . helpful-at-point)
			    (lisp-interaction-mode . helpful-at-point))))

;; common lisp
(use-package common-lisp-snippets)

;; racket
(use-package racket-mode)

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl")
  (+lookup-set-fn 'buffer '((lisp-mode . (lambda ()
					   (interactive)
					   (let ((tap  (thing-at-point 'symbol t)))
					     (slime-documentation tap))))))
  :hook
  (lisp-repl-mode . (lambda ()
		      (display-line-numbers-mode -1))))

(use-package lisp-mode
  :straight nil
  :mode "\\.lisp\\'"
  :hook (common-lisp-mode . (lambda ()
			      (interactive)
			      (slime-mode +1))))

(provide 'init-lisp)
