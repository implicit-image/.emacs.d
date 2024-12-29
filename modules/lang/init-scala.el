;;; -*- lexical-binding: t -*-

(use-package scala-mode
  :config
  (setq scala-indent:step 2
	scala-indent:align-parameters t))

(use-package sbt-mode)

(use-package lsp-metals
  :after scala-mode)

(provide 'init-scala)
