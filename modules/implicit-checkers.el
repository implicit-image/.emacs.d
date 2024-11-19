
(use-package flyspell)

(use-package flycheck
  :init
  (+windows/cfg '((flycheck-mode-major-mode)
		  :position bottom :height 0.3 )))

(use-package sideline-flycheck
  :after flycheck
  :init
  (setq sideline-backends-right '(sideline-flycheck))
  :hook
  (flycheck-mode . sideline-mode)
  (flycheck-mode . sideline-flycheck-setup))

(use-package flymake)


(provide 'implicit-checkers)
