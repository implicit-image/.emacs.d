;;; -*- lexical-binding: t -*-

(use-package treemacs
  :init
  (+windows-cfg '((treemacs-mode) :position left :width 0.18 :noselect t :stick t :dedicated t))
  (setq treemacs-indent-guide-style 'line
	treemacs-no-png-images t
	treemacs-eldoc-display 'detailed
	treemacs-indentation 1)
  :config
  (treemacs-load-theme "Default")
  :hook
  (treemacs-mode . (lambda ()
		     (interactive)
		     (treemacs-project-follow-mode +1)))
  :general
  (+leader-keys
    "TAB" '("Switch to project tree" . treemacs-select-window)
    "o p" '("Sidebar" . treemacs))
  (evil-treemacs-state-map
   "SPC TAB" 'evil-window-next))

(use-package treemacs-projectile
  :demand
  :after (treemacs projectile))

(use-package treemacs-magit
  :demand
  :after (treemacs magit))

(provide 'init-treemacs)
