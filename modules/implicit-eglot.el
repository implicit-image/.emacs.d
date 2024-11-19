

(use-package eglot
  :straight nil
  :init
  (setq eglot-autoshutdown t
	eglot-extend-to-xref t)
  (+windows/cfg '(("\*eldoc\*")
		  :regexp t :height 0.3 :position bottom :dedicated nil))
  :hook
  (eglot-mode . (lambda ()
		 (interactive)
		 (eldoc-mode +1)
		 (eldoc-box-hover-at-point-mode +1))))

(use-package eglot-booster
  :straight (eglot-booster :type git
			   :host github
			   :repo "jdtsmith/eglot-booster")
  :after eglot
  :config
  (eglot-booster-mode))

(provide 'implicit-eglot)
