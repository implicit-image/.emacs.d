;;; -*- lexical-binding: t -*-

(use-package java-ts-mode
  :preface
  (defun +jvm/set-up-java-environment ()
    "Set up java environment variables and locations."
    (require 'f)
    (setq lsp-java-java-path
	  (f-join (exec-path-from-shell-getenv "JAVA_HOME") "/bin/java")
	  lsp-java-import-gradle-java-home
	  (f-join (exec-path-from-shell-getenv "JAVA_HOME") "/bin/java")))
  :config
  (setq java-ts-mode-indent-offset 4)
  :hook
  (java-ts-mode . +jvm/set-up-java-environment))

(use-package lsp-java
  :init
  (setq lsp-java-references-code-lens-enabled t
	lsp-java-signature-help-enabled t
	lsp-java-signature-help-description-enabled t
	lsp-java-save-actions-organize-imports t
	lsp-java-completion-enabled t
	lsp-java-completion-overwrite t
	lsp-java-import-gradle-enabled t))

(use-package kotlin-mode
  :preface
  (defun +jvm/set-up-kotlin-environment ()
    "Set up kotlin environment variables and locations.")
  :config
  (setq kotlin-tab-width 4)
  :hook
  (kotlin-mode . +jvm/set-up-kotlin-environment))

(use-package scala-mode
  :config
  (setq scala-indent:step 2
	scala-indent:align-parameters t))

(use-package sbt-mode)

(use-package lsp-metals
  :after scala-mode)

(use-package groovy-mode
  :preface
  (defun +jvm/set-up-groovy-environment ()
    "Set up groovy environment variables and locations.")
  :hook
  (groovy-mode . +jvm/set-up-groovy-environment))

(use-package clojure-mode
  :preface
  (defun +jvm/set-up-clojure-environment ()
    "Set uo clojure environment depending on a runtime.")
  :hook
  (clojure-mode . +jvm/set-up-clojure-environment))

(use-package cider)

(use-package clojure-snippets)

(use-package clj-refactor)

(provide 'init-jvm)
