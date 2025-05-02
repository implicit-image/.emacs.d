;;; -*- lexical-binding: t -*-

(require 'rx)

(use-package haskell-mode
  :init
  (setq haskell-process-log t
	haskell-process-type 'auto
	haskell-compiler-type 'auto))

(use-package haskell-ts-mode
  :mode (rx (or "\\.hs\\'" "\\.lhs\\'")))

(use-package lsp-haskell
  :init
  (setq lsp-haskell-server-path "haskell-language-server-wrapper"
	;; lsp-haskell-server-args '()
	lsp-haskell-check-project nil
	lsp-haskell-completion-in-comments nil
	lsp-haskell-plugin-eval-global-on nil
	lsp-haskell-plugin-semantic-tokens-global-on t))

(provide 'init-haskell)
