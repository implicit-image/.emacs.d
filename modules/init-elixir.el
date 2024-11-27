(use-package elixir-ts-mode
  :mode (rx (or "\\.exs\\'" "\\.ex\\'"))
  :init (add-to-list '+lsp/servers-to-install 'elixir-ls)
  :config
  (setq elixir-basic-offset 4
	;; lsp config
	lsp-elixir-dialyzer-enabled t))

(use-package mix
  :hook (elixir-mode . mix-minor-mode))

(provide 'init-elixir)
