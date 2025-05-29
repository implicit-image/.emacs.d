;;; -*- lexical-binding: t -*-

;; (use-package eglot
;;   :straight nil
;;   :init
;;   (setq eglot-autoshutdown t
;;         eglot-extend-to-xref t)
;;   :config
;;   (add-to-list 'eglot-server-programs
;;                '(haskell-ts-mode . ("haskell-language-server-wrapper" "--lsp")))
;;   :hook
;;   (eglot-mode . (lambda ()
;;                   (interactive)
;;                   (eldoc-mode +1)
;;                   (eldoc-box-hover-at-point-mode +1))))
;; :bind*
;; ( :map eglot-mode-map
;;   ("C-x SPC c R" . eglot-rename)
;;   ("C-x SPc c F" . eglot-format)))

(setopt eglot-autoshutdown t
        eglot-extend-to-xref t)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(haskell-ts-mode . ("haskell-language-server-wrapper" "--lsp"))))

(add-hook 'eglot-mode 'eldoc-mode)

(provide 'init-eglot)
