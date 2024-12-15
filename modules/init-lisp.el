;;; -*- lexical-binding: t -*-

(use-package lispy
  :commands
  lispy-mode
  :init
  (add-to-list '+editing/evil-mc-incompatible-modes 'lispy-mode)
  :hook ((emacs-lisp-mode common-lisp-mode) . lispy-mode)
  :general
  (lispy-mode-map
   :states '(insert)
   "M-<" 'lispy-move-left
   "M->" 'lispy-move-right
   "M-k" 'lispy-kill-sentence))

;; emacs lisp
(use-package emacs-lisp-mode
  :straight nil
  :init
  (+lookup-set-fn 'buffer
		  '(emacs-lisp-mode . helpful-at-point)
		  '(lisp-interaction-mode . helpful-at-point)))

;; common lisp
(use-package common-lisp-snippets)

;; racket
(use-package racket-mode)

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl")
  (+lookup-set-fn 'buffer '(lisp-mode . (lambda ()
					   (interactive)
					   (let ((tap  (thing-at-point 'symbol t)))
					     (slime-documentation tap)))))
  :general
  (slime-popup-buffer-mode-map
   :states '(normal)
   "q" 'quit-window))

(use-package lisp-mode
  :straight nil
  :mode "\\.lisp\\'"
  :hook (common-lisp-mode . (lambda ()
			      (interactive)
			      (slime-mode +1))))

(provide 'init-lisp)
