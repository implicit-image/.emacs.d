;;;; -*- lexical-binding: t -*-
;;;; Support for Bioware's Neverwinter Script language.

(use-package lsp-nwscript
  :straight (lsp-nwscript :type git
			  :host github
			  :repo "implicit-image/lsp-nwscript.el"
			  :files ("lsp-nwscript.el")))

(provide 'init-nwscript)
