(use-package flyspell)

(use-package flycheck
  :init
  (+windows-cfg '((flycheck-mode-major-mode)
		  :position bottom :height 0.3 ))
  :hook
  ((lsp-mode emacs-lisp-mode) . flycheck-mode)
  :general
  (flycheck-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "s e" '("Flycheck errors" . counsel-flycheck))
  (flycheck-mode-map
   :states '(normal visual insert)
   "C-c ]" 'flycheck-next-error
   "C-c [" 'flycheck-previous-error))

(use-package sideline-flycheck
  :after flycheck
  :init
  (setq sideline-backends-right '(sideline-flycheck)
	sideline-flycheck-max-lines 2)
  :hook
  (flycheck-mode . sideline-mode)
  (flycheck-mode . sideline-flycheck-setup))

(use-package flymake)

(provide 'init-checkers)
