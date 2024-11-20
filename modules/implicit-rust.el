(use-package cargo-mode
  :hook
  (rust-mode . cargo-minor-mode)
  :config
  (setq compilation-scroll-output t))

(use-package rustic
  :mode "\\.rs\\'"
  :config
  (setq rustic-indent-offset 4
	rustic-lsp-server "rust-analyzer"
	;; formatting
	rustic-format-trigger 'on-save)
  :hook
  (rustic-mode . (lambda ()
		   (lsp))))

(use-package ob-rust
  :after org)

(use-package rust-ts-mode
  :init
  (setq rust-ts-mode-indent-offset 4))


(provide 'implicit-rust)
