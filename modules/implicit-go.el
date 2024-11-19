
(use-package go-ts-mode
  :mode "\\.go\\'"
  :init
  (setq go-ts-mode-indent-offset 4)
  :hook
  (go-mode . (lambda ()
	       (lsp)
	       (flycheck-mode +1))))


(provide 'implicit-go)
