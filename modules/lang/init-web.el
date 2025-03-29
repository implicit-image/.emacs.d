;;; -*- lexical-binding: t -*-

(use-package js2-mode)

(use-package js-ts-mode
  :straight nil)

(use-package rjsx-mode)

(use-package typescript-ts-mode
  :straight nil
  :init
  (setq typescript-ts-mode-indent-offset 2))

(use-package css-ts-mode
  :straight nil)

(use-package json-ts-mode
  :mode "\\.json\\'"
  :init
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . json-ts-mode)))

(use-package tide
  :init
  (+windows-cfg '(("\*tide-documentation\*") :position bottom :height 0.3))
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

(use-package lsp-tailwindcss
  :straight (lsp-tailwindcss :type git
                             :host github
                             :repo "merrickluo/lsp-tailwindcss")
  :init
  (setq lsp-tailwindcss-addon-mode t)
  :config
  (dolist (tw-major-mode
           '(css-mode
             css-ts-mode
             typescript-mode
             typescript-ts-mode
             tsx-ts-mode
             js2-mode
             js-ts-mode
             clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

(provide 'init-web)
