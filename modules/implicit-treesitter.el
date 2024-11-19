(use-package treesit-auto
  :demand
  :init
  (setq treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package evil-textobj-tree-sitter
  :after (treeesit evil))

(provide 'implicit-treesitter)
