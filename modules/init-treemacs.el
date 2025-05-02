;;; -*- lexical-binding: t -*-

(use-package treemacs
  :init
  (+windows-cfg '((treemacs-mode) :position left :width 20 :noselect t :stick t :dedicated t))
  (setq treemacs-indent-guide-style 'line
        treemacs-no-png-images t
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
    "o p" '("Sidebar" . treemacs))
  (evil-treemacs-state-map
   "SPC w" 'evil-window-next
   "C-w" 'evil-windw-next
   "C-l" 'evil-window-right
   "C-k" 'evil-window-up
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down))

(use-package treemacs-projectile
  :demand
  :after (treemacs projectile))

(use-package treemacs-magit
  :demand
  :after (treemacs magit))

(provide 'init-treemacs)
