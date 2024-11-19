
(use-package erlang
  :init (add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
  :hook ((erlang-mode) . (lambda ()
			   (lsp))))

(use-package otp)

(provide 'implicit-erlang)
