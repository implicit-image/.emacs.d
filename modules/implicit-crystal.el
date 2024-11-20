(use-package crystal-mode
  :init
  (setq crystal-indent-level 4)
  :hook ((crystal-mode) . (lambda ()
			    (interactive)
			    (lsp))))

(provide 'init-crystal)
