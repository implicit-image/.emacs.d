;;; -*- lexical-binding: t -*-

(use-package java-ts-mode
  :config
  (setq java-ts-mode-indent-offset 4))

(use-package lsp-java)

(use-package kotlin-mode
  :config
  (setq kotlin-tab-width 4)
  :hook (kotlin-mode . (lambda ()
			 (exec-path-from-shell--standard-shell-p))))

(use-package scala-mode
  :config
  (setq scala-indent:step 2
	scala-indent:align-parameters t))

(use-package sbt-mode)

(use-package lsp-metals
  :after scala-mode)

(use-package groovy-mode)

(use-package clojure-mode)

(use-package cider)

(use-package clojure-snippets)

(use-package clj-refactor)

(provide 'init-jvm)
