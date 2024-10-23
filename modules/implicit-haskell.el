
(use-package haskell-mode
  :init
  (setq haskell-process-log t
	haskell-process-type 'stack-ghci))


(use-package lsp-haskell)

;; (use-package dante)

(provide 'implicit-haskell)
