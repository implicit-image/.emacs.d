
(use-package haskell-mode
  :init
  (setq haskell-process-log t
	haskell-process-type 'stack-ghci
	haskell-compiler-type 'stack))


(use-package lsp-haskell
  :init
  (setq lsp-haskell-server-path (expand-file-name "~/.ghcup/bin/haskell-language-server-wrapper")
	lsp-haskell-max-completions 80
	lsp-haskell-plugin-ghcide-completions-global-on nil)
  :hook
  (haskell-mode . (lambda ()
		    (require 'lsp-haskell))))

(use-package dante)

(provide 'implicit-haskell)
