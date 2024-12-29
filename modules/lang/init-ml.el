;;; -*- lexical-binding: t -*-

(use-package reason-mode)

(use-package dune)

(use-package merlin
  :init
  (setq merlin-completion-with-doc t
	merlin-completion-dwim t)
  :hook
  (tuareg-mode . merlin-mode))

(use-package flycheck-ocaml
  :hook
  ((merlin-mode reason-mode) . (lambda ()
				 (interactive)
				 (setq-local merlin-error-after-save nil)
				 (flycheck-ocaml-setup))))

(use-package merlin-eldoc
  :init
  (+lookup-set-fn 'popup '(tuareg-mode . eldoc-doc-buffer))
  (defun +ml/setup-merlin-doc ()
    "Setup local variables controlling eldoc documentation."
    (progn
      (setq-local eldoc-echo-area-use-multiline-p t
		  merlin-eldoc-max-lines 10)))
  (setq merlin-eldoc-max-lines 10
	merlin-eldoc-delimiter "  \n  ")
  :hook
  ((reason-mode tuareg-mode) . merlin-eldoc-setup)
  ((merlin-mode) . +ml/setup-merlin-doc))

(use-package ocaml-ts-mode)

(use-package tuareg
  :init
  (setq tuareg-browser 'browse-url-firefox
	tuareg-default-indent 4
	tuareg-match-patterns-aligned t
	;; opam
	tuareg-opam-insinuate t
	tuareg-opam-indent-basic 4))

(provide 'init-ml)
