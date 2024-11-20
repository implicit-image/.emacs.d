(use-package ada-mode
  :preface(add-to-list '+lsp/servers-to-install 'ada-ls)
  :init
  (setq ada-indent-use 4
	ada-indent-when 4)
  :hook ((ada-mode) . (lambda ()
			(interactive)
			(lsp))))


(provide 'implicit-ada)
