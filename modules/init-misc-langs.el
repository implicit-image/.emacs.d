;;; -*- lexical-binding: t -*-

(use-package nwscript-mode
  :mode "\\.nss\\'"
  :commands
  nwscript-mode
  :straight (nwscript-mode :type git
			   :host github
			   :repo "implicit-image/nwscript-mode.el"
			   :files ("nwscript-mode.el"))
  :init
  (setq nwscript-mode-indent-level 4))

(use-package lsp-nwscript
  :straight (lsp-nwscript :type git
			  :host github
			  :repo "implicit-image/lsp-nwscript.el"
			  :files ("lsp-nwscript.el"))
  :after nwscript-mode
  :init)

(use-package gdscript-mode)

(use-package lua-mode)

(use-package nim-mode)

(use-package d-mode)

(use-package purescript-mode)

(provide 'init-misc-langs)
