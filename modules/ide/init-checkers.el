;;; -*- lexical-binding: t -*-

(use-package flycheck
  :custom
  (flycheck-indication-mode nil)
  (flycheck-highlighting-mode 'symbols)
  (flycheck-idle-check-delay 1.0)
  :init
  (+windows-cfg '((flycheck-mode-major-mode)
		  :position bottom :height 0.3 ))
  :hook
  ((lsp-mode emacs-lisp-mode merlin-mode) . flycheck-mode)
  :general
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

(use-package consult-flycheck
  :straight (consult-flycheck :type git
			      :host github
			      :repo "minad/consult-flycheck")
  :general
  (flycheck-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "s e" '("Flycheck errors" . consult-flycheck)))

(use-package flymake)

(use-package flyspell)

(provide 'init-checkers)
