;;; -*- lexical-binding: t -*-

(require 'rx)

(use-package js2-mode)

(use-package js-ts-mode
  :straight nil
  :init (add-to-list '+lsp/servers-to-install 'ts-ls))

(use-package rjsx-mode)

(use-package typescript-ts-mode
  :straight nil
  :init
  (setq typescript-ts-mode-indent-offset 2)
  :hook
  (typescript-ts-mode . lsp)
  (tsx-ts-mode . lsp))

(use-package css-ts-mode
  :straight nil)

(use-package json-ts-mode
  :mode "\\.json\\'"
  :init (add-to-list '+lsp/servers-to-install 'json-ls))

(use-package tide
  :init
  (+windows-cfg '(("\*tide-documenation\*") :position bottom :height 0.3))
  (+lookup-set-fn 'buffer '(typescript-ts-mode . tide-documentation-at-point))
  :config
  (setq tide-enable-xref t
	tide-imenu-flatten t
	tide-completion-detailed t)
  :hook ((typescript-ts-mode) . (lambda ()
				  (interactive)
				  (tide-setup)
				  (tide-hl-identifier-mode +1))))

(use-package web-mode
  :mode "\\.html\\'"
  :config
  (setq web-mode-enable-auto-expanding t))

;; rest api interaction in org mode
(use-package verb)

(use-package php-mode
  :mode "\\.php\\'")

(use-package composer
  :hook ((php-mode)) . #'composer)

(use-package know-your-http-well)

(provide 'init-web)
