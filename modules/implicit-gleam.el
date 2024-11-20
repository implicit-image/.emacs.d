(use-package gleam-ts-mode
  :mode "\\.gleam\\'"
  :config
  (setq gleam-ts-indent-offset 4)
  :hook
  (gleam-ts-mode . (lambda () (lsp))))

(provide 'implicit-gleam)
