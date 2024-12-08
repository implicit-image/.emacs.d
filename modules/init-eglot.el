;;; -*- lexical-binding: t -*-

(use-package eglot
  :straight nil
  :init
  (setq eglot-autoshutdown t
	eglot-extend-to-xref t)
  (+windows-cfg '(("\*eldoc\*")
		  :regexp t :height 0.3 :position bottom :dedicated nil))
  :config
  (add-to-list 'eglot-server-programs
		'(haskell-ts-mode . ("haskell-language-server-wrapper" "--lsp")))
  :hook
  (eglot-mode . (lambda ()
		  (interactive)
		  (eldoc-mode +1)
		  (eldoc-box-hover-at-point-mode +1)))
  :general
  (eglot-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "c R" '("Rename" . eglot-rename))
  (eglot-mode-map
   :states '(visual)
   "=" '("Format" . eglot-format)))

(use-package eglot-booster
  :straight (eglot-booster :type git
			   :host github
			   :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))

(provide 'init-eglot)
