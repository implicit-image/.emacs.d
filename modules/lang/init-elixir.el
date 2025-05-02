;;; -*- lexical-binding: t -*-

(use-package elixir-ts-mode
  :mode (rx (or "\\.exs\\'" "\\.ex\\'"))
  :config
  (setq elixir-basic-offset 4
	;; lsp config
	lsp-elixir-dialyzer-enabled t))

(use-package mix
  :hook (elixir-mode-hook . mix-minor-mode))

(provide 'init-elixir)
