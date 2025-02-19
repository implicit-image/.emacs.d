;;;; -*- lexical-binding: t -*-
;;;; Support for Bioware's Neverwinter Script language.

(use-package lsp-nwscript
  :straight (lsp-nwscript :type git
			  :host github
			  :repo "implicit-image/lsp-nwscript.el"
			  :files ("lsp-nwscript.el")))

(use-package nwscript-mode
  :straight (nwscript-mode :type git
			   :host github
			   :repo "implicit-image/nwscript-mode.el"))

(provide 'init-nwscript)
