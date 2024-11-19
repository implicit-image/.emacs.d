;; TODO: fix tree sitter grammar url
;;(use-package java-ts-mode)

;; (use-package java-ts-mode
;;   :straight nil)

(use-package java-ts-mode
  :config
  (setq java-ts-mode-indent-offset 4)
  :hook (java-ts-mode . (lambda ()
			  (eglot-ensure))))

(use-package kotlin-mode
  :config
  (setq kotlin-tab-width 4)
  :hook (kotlin-mode . (lambda ()
			 (exec-path-from-shell--standard-shell-p)
			 (lsp))))

(use-package scala-mode
  :config
  (setq scala-indent:step 2
	scala-indent:align-parameters t)
  :hook (scala-mode . (lambda ()
			(lsp))))

(use-package sbt-mode)

(use-package lsp-metals
  :config
  (setq lsp-metals-java-home )
  :after scala-mode)


(use-package groovy-mode)

(use-package clojure-mode)

(use-package cider)

(use-package clojure-snippets)

(use-package clj-refactor)

(provide 'implicit-jvm)
