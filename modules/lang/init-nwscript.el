;;;; -*- lexical-binding: t -*-
;;;; Support for Bioware's Neverwinter Script language.


(require 'rx)

(use-package lsp-nwscript
  :straight (lsp-nwscript :type git
                          :host github
                          :repo "implicit-image/lsp-nwscript.el"
                          :files ("lsp-nwscript.el")))

(use-package nwscript-mode
  :autoload (nwscript-mode)
  :mode "\\.nss\\'"
  :straight (nwscript-mode :type git
                           :host github
                           :branch "master"
                           :repo "implicit-image/nwscript-mode.el")
  :hook
  (nwscript-mode . (lambda ()
                     (setq-local case-fold-search nil)
		     (indent-tabs-mode -1))))

(provide 'init-nwscript)
