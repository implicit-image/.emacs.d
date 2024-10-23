(use-package evil-mc)

(use-package evil-multiedit)

(use-package evil-nerd-commenter
  :after evil)

(use-package editorconfig
  :demand
  :config
  (editorconfig-mode 1))

(use-package undo-tree
  :demand
  :init
  (require 'f)
  (setq undo-tree-history-directory-alist `(("." . ,(f-join user-emacs-directory "undo-tree/"))))
  :config
  (global-undo-tree-mode 1)
  (push '(" *undo-tree*"
	  :width 0.2
	  :position left) popwin:special-display-config))


(provide 'implicit-editing)
