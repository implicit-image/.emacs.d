;;; -*- lexical-binding: t -*-

(use-package treemacs
  :init
  (setq treemacs-indent-guide-style 'line
        treemacs-no-png-images t
        treemacs-icons nil
        treemacs-eldoc-display 'detailed
        treemacs-indentation 1)

  (when (bound-and-true-p eldoc-box-hover-mode)
    (add-to-list '+eldoc-minibuffer-display-modes 'treemacs-mode))
  :config
  (treemacs-load-theme "Default")
  :hook
  (treemacs-mode-hook . (lambda ()
                          (interactive)
                          (toggle-truncate-lines -1)
                          (treemacs-project-follow-mode +1)))
  :general
  (+leader-keys
    "TAB" '("Switch to project tree" . treemacs-select-window)
    "o p" '("Sidebar" . treemacs)))

(use-package treemacs-projectile
  :demand
  :after (treemacs projectile))

(use-package treemacs-magit
  :demand
  :after (treemacs magit))

(provide 'init-treemacs)
