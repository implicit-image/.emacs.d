(require 'rx)

(use-package js2-mode)

(use-package js-ts-mode
  :straight nil
  :init (add-to-list '+lsp/servers-to-install 'ts-ls)
  :mode (rx  (or "\\.js\\'" "\\.jsm\\'"))
  :hook ((js-ts-mode) . (lambda ()
			  (lsp))))

(use-package rjsx-mode
  :mode (rx (or "\\.tsx\\'" "\\.jsx\\'"))
  :hook ((rjsx-mode) . (lambda ()
			 (lsp))))

(use-package json-ts-mode
  :mode "\\.json\\'"
  :init (add-to-list '+lsp/servers-to-install 'json-ls)
  :hook (json-mode . (lambda ()
		       (lsp))))

(use-package tide
  :init
  (+windows-cfg '(("\*tide-documenation\*") :position bottom :height 0.3))
  (+lookup-set-fn 'buffer '(typescript-ts-mode . tide-documentation-at-point))
  :config
  (setq tide-enable-xref t)
  :hook ((typescript-ts-mode) . (lambda ()
				  (interactive)
				  (tide-setup)
				  (tide-hl-identifier-mode +1))))

(use-package web-mode
  :mode "\\.html\\'"
  :config
  (setq web-mode-enable-auto-expanding t)
  :hook ((web-mode) . (lambda ()
			(lsp))))

;; rest api interaction in org mode
(use-package verb)

(use-package php-mode
  :mode "\\.php\\'"
  :hook ((php-mode) . (lambda ()
			(lsp))))

(use-package composer
  :hook ((php-mode)) . #'composer)

(use-package know-your-http-well)

(provide 'init-web)
