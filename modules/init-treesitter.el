;;; -*- lexical-binding: t -*-

(use-package treesit-auto
  :commands
  (global-treesit-auto-mode)
  :init
  (setq treesit-auto-install t)
  :hook
  (after-init . (lambda ()
		  (interactive)
		  (require 'treesit-auto)
		  (treesit-auto-add-to-auto-mode-alist 'all)
		  (global-treesit-auto-mode))))


(use-package evil-textobj-tree-sitter
  :after (treeesit evil))

(provide 'init-treesitter)
