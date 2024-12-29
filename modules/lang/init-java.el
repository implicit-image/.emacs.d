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


(provide 'init-java)
