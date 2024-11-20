(use-package treemacs
  :demand
  :init
  (+windows-cfg '((treemacs-mode) :position left :width 0.18 :noselect t :stick t :dedicated t))
  (setq treemacs-indent-guide-style 'line
	treemacs-indentation 1
	treemacs-pre-refresh-hook)
  :config
  (treemacs-load-theme "Default")
  :hook
  (treemacs-mode . (lambda ()
		     (interactive)
		     (display-line-numbers-mode -1)
		     (treemacs-project-follow-mode +1))))

(use-package treemacs-projectile
  :demand
  :after (treemacs projectile))

(use-package treemacs-magit
  :demand
  :after (treemacs magit))

(use-package treemacs-all-the-icons)

(provide 'implicit-treemacs)
