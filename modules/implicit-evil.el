(use-package evil
  :demand
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-undo-system 'undo-tree
	evil-lookup-func 'woman)
  :config
  (evil-mode 1))

(use-package evil-collection
  :demand
  :config
  (evil-collection-init evil-collection-mode-list))

(use-package evil-goggles
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package vimish-fold
  :after evil)

(use-package evil-vimish-fold
  :after vimish-fold
  :init
  (setq evil-vimish-fold-target-modes '(prog-mode markdown-mode conf-mode text-mode))
  :config
  (global-evil-vimish-fold-mode 1))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package treemacs-evil
  :demand
  :after treemacs)


(provide 'implicit-evil)
