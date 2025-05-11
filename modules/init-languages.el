;;; -*- lexical-binding: t -*-

;;;; c
(use-package cc-mode)

(use-package c-ts-mode
  :mode "\\.c\\'"
  :init
  (setq c-ts-mode-indent-offset 4))

(use-package rtags)

(use-package ccls
  :init
  (setq ccls-executable "ccls"))

;;;; javascript
(use-package js-ts-mode
  :straight nil)

(use-package rjsx-mode)

(use-package typescript-ts-mode
  :straight nil
  :init
  (setq typescript-ts-mode-indent-offset 2))

;;;; css
(use-package css-ts-mode
  :straight nil)

;;;; json
(use-package json-ts-mode
  :mode "\\.json\\'"
  :init
  (add-to-list 'auto-mode-alist '("\\.cjs\\'" . json-ts-mode)))

;;;; typescript
(use-package tide
  :init
  (+lookup-set-fn 'buffer '(typescript-ts-mode . tide-documentation-at-point))
  :config
  (setq tide-enable-xref t
        tide-imenu-flatten t
        tide-completion-detailed t)
  :hook (typescript-ts-mode-hook . (lambda ()
                                     (interactive)
                                     (tide-setup)
                                     (tide-hl-identifier-mode +1))))

;;;; web mode
(use-package web-mode
  :mode "\\.html\\'"
  :config
  (setq web-mode-enable-auto-expanding t))


;;;; php
(use-package php-ts-mode
  :mode "\\.php\\'")

(use-package composer
  :hook (php-mode-hook . composer))

;;;; web info
(use-package verb)

(use-package know-your-http-well)

;;;; python
(use-package anaconda-mode)


(use-package python-ts-mode
  :straight nil
  :init
  (setq python-indent-offset 4))

(use-package pyvenv)


;;;; scala
(use-package scala-mode
  :config
  (setq scala-indent:step 4
        scala-indent:align-parameters t))

(use-package sbt-mode)

;;;; java
(use-package java-ts-mode
  :preface
  (defun +jvm/set-up-java-environment ()
    "Set up java environment variables and locations."
    (setq lsp-java-java-path
          (expand-file-name (file-name-concat (exec-path-from-shell-getenv "JAVA_HOME") "/bin/java"))
          lsp-java-import-gradle-java-home
          (expand-file-name (file-name-concat (exec-path-from-shell-getenv "JAVA_HOME") "/bin/java"))))
  :config
  (setq java-ts-mode-indent-offset 4)
  :hook
  (java-ts-mode-hook . +jvm/set-up-java-environment))

;;;; kotlin
(use-package kotlin-mode
  :preface
  (defun +jvm/set-up-kotlin-environment ()
    "Set up kotlin environment variables and locations.")
  :config
  (setq kotlin-tab-width 4)
  :hook
  (kotlin-mode-kotlin . +jvm/set-up-kotlin-environment))

;;;; groovy
(use-package groovy-mode
  :preface
  (defun +jvm/set-up-groovy-environment ()
    "Set up groovy environment variables and locations.")
  :hook
  (groovy-mode-hook . +jvm/set-up-groovy-environment))

;;;; clojure
(use-package clojure-mode
  :preface
  (defun +jvm/set-up-clojure-environment ()
    "Set uo clojure environment depending on a runtime.")
  :hook
  (clojure-mode-hook . +jvm/set-up-clojure-environment))

(use-package cider)

(use-package clojure-snippets)

(use-package clj-refactor)

;;;; haskell
(use-package haskell-mode
  :init
  (setq haskell-process-log t
        haskell-process-type 'auto
        haskell-compiler-type 'auto))

(use-package haskell-ts-mode
  :mode "\\.hs\\|.lhs\\'")

;;;; rust
(use-package cargo-mode
  :config
  (setq compilation-scroll-output t)
  :hook
  (rust-mode-hook . cargo-minor-mode))

(use-package rustic
  :config
  (setq rustic-indent-offset 4
        rustic-lsp-server "rust-analyzer"
        rustic-lsp-client 'lsp-mode
        ;; formatting
        rustic-format-trigger 'on-save))


(use-package rust-ts-mode
  :init
  (setq rust-ts-mode-indent-offset 4))

;;;; lisp
(use-package lispy
  :disabled
  :commands
  lispy-mode
  :hook
  ((emacs-lisp-mode-hook common-lisp-mode-hook scheme-mode-hook racket-mode-hook dune-mode-hook) . lispy-mode))
;; :general
;; (lispy-mode-map
;;  :states '(insert emacs)
;;  "C-c g" 'lispy-ace-paren)
;; (lispy-mode-map
;;  :states '(insert emacs normal)
;;  "C-c <" 'lispy-move-left
;;  "C-c >" 'lispy-move-right
;;  "C-M-[" 'lispy-wrap-brackets
;;  "C-M-{" 'lispy-wrap-braces
;;  "C-M-(" 'lispy-wrap-round))

;; emacs lisp
(use-package emacs-lisp-mode
  :straight nil
  :init
  (+lookup-set-fn 'buffer
                  '(emacs-lisp-mode . helpful-at-point)
                  '(lisp-interaction-mode . helpful-at-point)))

(use-package lisp
  :straight nil
  :init
  (setq delete-pair-blink-delay 0.03))

;; racket
(use-package racket-mode)

;; scheme
(use-package geiser)

;; common-lisp
(use-package sly
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package common-lisp-snippets)

(use-package lisp-mode
  :straight nil
  :mode "\\.lisp\\'"
  :hook (common-lisp-mode-hook . (lambda ()
                                   (interactive)
                                   (slime-mode +1))))

;;;; dart
(use-package dart-mode)

;;;; purescript
(use-package purescript-mode)

;;;; R
(use-package ess)

;;;; gleam
(use-package gleam-ts-mode
  :mode "\\.gleam\\'"
  :config
  (setq gleam-ts-indent-offset 4))

;;;; erlang
(use-package erlang
  :mode "\\.erl\\'")

(use-package otp)

(use-package edts)

;;;; elixir
(use-package elixir-ts-mode
  :mode (rx (or "\\.exs\\'" "\\.ex\\'"))
  :config
  (setq elixir-basic-offset 4
        ;; lsp config
        lsp-elixir-dialyzer-enabled t))

(use-package mix
  :hook (elixir-mode-hook . mix-minor-mode))

;;;; go
(use-package go-ts-mode
  :mode "\\.go\\'"
  :init
  (setq go-ts-mode-indent-offset 4))

;;;; ruby
(use-package enh-ruby-mode)

(use-package robe)

;;;; dotnet
(use-package csharp-ts-mode
  :straight nil
  :mode "\\.cs\\'")

(use-package csproj-mode)

(use-package sln-mode)

(use-package shader-mode)

(use-package sharper)

(use-package fsharp-mode
  :mode "\\.fs\\'")

(use-package fstar-mode)

;;;; zig
(use-package zig-mode
  :mode "\\.zig\\'")

;;;; assemblers
(use-package mips-mode)

(use-package riscv-mode)

(use-package asm-mode
  :straight nil)

(use-package fasm-mode)

(use-package masm-mode)

(use-package nasm-mode)

;;;; idris
(use-package idris-mode)

;;;; ada
(use-package ada-mode
  :init
  (setq ada-indent-use 4
        ada-indent-when 4))


;;;; Editing GNAT files.
(use-package gpr-mode)

;;;; elm
(use-package elm-mode
  :init
  (setq elm-reactor-port 6969
        elm-indent-offset 4
        elm-format-on-save t))

;;;; lua
(use-package lua-ts-mode)

;;;; nim
(use-package nim-mode)

;;;; gdscript
(use-package gdscript-ts-mode
  :straight nil)

;;;; d
(use-package d-mode)

;;;; lean
(use-package lean-mode)

;; support for Lean4 theorem prover
(use-package lean4-mode
  :commands lean4-mode
  :straight (lean4-mode :type git :host github
                        :repo "leanprover-community/lean4-mode"
                        :files ("*.el" "data")))

;;;; sql
(use-package sql-mode
  :straight nil)

;;;; solidity
(use-package solidity-mode)

;;;; languages for nand2tetris course
(use-package init-nand2tetris
  :commands
  (jack-mode hdl-mode tst-mode vm-mode)
  :straight (init-nand2tetris :type git
                              :host github
                              :repo "Deng-Li3/emacs-nand2tetris"))

;;;; ml family languages
(use-package reason-mode)

(use-package dune)

(use-package merlin
  :init
  (setq merlin-completion-with-doc t
        merlin-completion-dwim t)
  :hook
  (tuareg-mode-hook . merlin-mode))

(use-package flycheck-ocaml
  :hook
  ((merlin-mode-hook reason-mode-hook) . (lambda ()
                                           (interactive)
                                           (setq-local merlin-error-after-save nil)
                                           (flycheck-ocaml-setup))))

(use-package merlin-eldoc
  :init
  (+lookup-set-fn 'popup '(tuareg-mode . eldoc-doc-buffer))
  (defun +ml/setup-merlin-doc ()
    "Setup local variables controlling eldoc documentation."
    (progn
      (setq-local eldoc-echo-area-use-multiline-p t
                  merlin-eldoc-max-lines 10)))
  (setq merlin-eldoc-max-lines 10
        merlin-eldoc-delimiter "  \n  ")
  :hook
  ((reason-mode-hook tuareg-mode-hook) . merlin-eldoc-setup)
  ((merlin-mode-hook) . +ml/setup-merlin-doc))

(use-package ocaml-ts-mode)

(use-package tuareg
  :init
  (setq tuareg-browser 'browse-url-firefox
        tuareg-default-indent 4
        tuareg-match-patterns-aligned t
        ;; opam
        tuareg-opam-insinuate t
        tuareg-opam-indent-basic 4))

;;;; LaTeX editing
(use-package auctex)

(use-package cdlatex
  :hook
  (LaTeX-mode-hook . turn-on-cdlatex)
  (org-mode-hook . org-cdlatex-mode))

(use-package xenops)

;;;; various shell languages
(use-package bash-ts-mode
  :straight nil)

(use-package nushell-mode)

(use-package powershell)

;;;; common configuration languages
(use-package conf-mode
  :straight nil
  :init
  (add-to-list 'auto-mode-alist '("/git-rebase-todo\\'" . conf-mode)))

(use-package toml-ts-mode)

(use-package nxml-mode
  :straight nil
  :hook
  (nxml-mode-hook . +whitespace-off))

(use-package nix-mode)

;;;; cuda support
(use-package cuda-mode)

;;;; Bioware's Neverwinter Script support
(use-package nwscript-mode
  :autoload (nwscript-mode)
  :mode "\\.nss\\'"
  :straight (nwscript-mode :type git
                           :host github
                           :branch "master"
                           :repo "implicit-image/nwscript-mode.el")
  :init
  (defun +nwscript--setup ()
    (interactive)
    (setq-local case-fold-search t)
    (add-hook 'completion-at-point-functions (cape-capf-super (cape-company-to-capf 'company-dabbrev-code)
                                                              'cape-file))
    (funcall-interactively 'untabify (point-min) (point-max))
    (save-buffer)
    (indent-tabs-mode -1))
  :hook
  (nwscript-mode-hook . +nwscript--setup))
;; :general
;; (nwscript-mode-map
;;  :states 'normal
;;  :prefix "SPC"
;;  :non-normal-prefix "C-c SPC"
;;  "s o" '("Outline" . consult-outline)))

;;;; csv
(use-package csv-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.2da\\'" . csv-mode))

  (defun +csv/setup ()
    "setup `csv-mode' options for 2da files."
    (interactive)
    (cond ((string-equal (file-name-extension (buffer-file-name)) "2da")
           (csv-align-mode 1)
           ;; find row with id 0
           (search-forward-regexp "^0 ")
           ;; go line up to header line
           (previous-line)
           ;; set csv header
           (csv-header-line t))))

  :config
  (with-eval-after-load 'dabbrev
    (add-to-list 'dabbrev-ignored-buffer-modes 'csv-mode))

  :hook
  (csv-mode-hook . +csv/setup))

;;;; markdown
(use-package markdown-mode
  :init
  (defun +md-check-gfm ()
    (interactive)
    (if (eq ))))
;; :general
;; (gfm-view-mode-map
;;  :states 'normal
;;  "q" 'kill-this-buffer))

(use-package markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :defer 't
  :config
  (add-to-list 'treesit-language-source-alist '(markdown "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src"))
  (add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))

;;;; crystal

(use-package crystal-mode
  :init
  (setq crystal-indent-level 4))

;;;; plantuml

(use-package plantuml-mode)

(use-package flycheck-plantuml)


(provide 'init-languages)
