;;; -*- lexical-binding: t -*-

(use-package ccls
  :init
  (setq ccls-executable "ccls"))

(setq typescript-ts-mode-indent-offset 4
      go-ts-mode-indent-offset 4
      c-ts-mode-indent-offset 4
      java-ts-mode-indent-offset 4
      rust-ts-mode-indent-offset 4)

(add-to-list 'auto-mode-alist '("\\.cjs\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json[c]*\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))
(add-to-list 'auto-mode-alist '("\\/git-rebase-todo\\'" . conf-mode))

;; ;;;; typescript
;; (use-package tide
;;   :init
;;   (setq tide-enable-xref t
;;         tide-imenu-flatten t
;;         tide-completion-detailed t)
;;   :config
;;   (+lookup-set-fn! buffer (typescript-ts-mode . tide-documentation-at-point))
;;   :hook (typescript-ts-mode-hook . (lambda ()
;;                                      (interactive)
;;                                      (tide-setup)
;;                                      (tide-hl-identifier-mode +1))))

;;;; web mode
(use-package web-mode
  :mode "\\.html\\'"
  :config
  (setq web-mode-enable-auto-expanding t))

(use-package composer
  :hook (php-mode-hook . composer))

;;;; web info
(use-package verb)

(use-package pyvenv)

;;;; scala
(use-package scala-mode
  :init
  (setq scala-indent:step 4
        scala-indent:align-parameters t))

(use-package sbt-mode)

;;;; kotlin
(use-package kotlin-mode
  :init
  (setq kotlin-tab-width 4))

;;;; groovy
(use-package groovy-mode)

;;;; clojure
(use-package clojure-mode)

(use-package cider)

(use-package clojure-snippets)

(use-package clj-refactor)

(use-package haskell-ts-mode
  :custom
  (haskell-ts-font-lock-level 3)
  (haskell-ts-use-indent t)
  (haskell-ts-ghci "ghci")
  :mode "\\.hs\\|.lhs\\'"
  :config
  (add-to-list 'treesit-language-source-alist
               '(haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1")))
  (unless (treesit-grammar-location 'haskell)
    (treesit-install-language-grammar 'haskell)))

;;;; rust
(use-package cargo-mode
  :config
  (setq compilation-scroll-output t)
  :hook
  (rust-ts-mode-hook . cargo-minor-mode))

(with-eval-after-load 'elisp-mode
  (+lookup-set-fn! buffer (emacs-lisp-mode . helpful-at-point)))

(with-eval-after-load 'lisp-interaction-mode
  (+lookup-set-fn! buffer (lisp-interaction-mode . helpful-at-point)))

;; racket
(use-package racket-mode)

;; scheme
(use-package geiser)

;; common-lisp
(use-package sly
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package common-lisp-snippets)

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


;;;; ruby
(use-package enh-ruby-mode)

(use-package robe)

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

;;;; nim
(use-package nim-mode)

(use-package gdscript-mode)

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

(use-package merlin-eldoc
  :init
  (setq merlin-eldoc-max-lines 10
        merlin-eldoc-delimiter "  \n  ")
  :config
  (+lookup-set-fn! popup (tuareg-mode . eldoc-doc-buffer))
  :hook
  ((merlin-mode-hook) . +merlin-mode--setup))

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

(use-package nushell-mode)

(use-package powershell)

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
  :hook
  (nwscript-mode-hook . +nwscript-mode--setup))

;;;; csv
(use-package csv-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.2da\\'" . csv-mode))
  :hook
  (csv-mode-hook . +csv-mode--setup))

;;;; markdown
(use-package markdown-mode)

;; (use-package yuck-mode
;;   :straight nil
;;   :commands
;;   yuck-mode)

(use-package markdown-ts-mode
  :mode ("\\.md\\'" . markdown-ts-mode)
  :init
  (with-eval-after-load 'treesit
    (add-to-list 'treesit-language-source-alist
                 '(markdown . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown/src")))
    (add-to-list 'treesit-language-source-alist
                 '(markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src")))))

(use-package crystal-mode
  :init
  (setq crystal-indent-level 4))

(use-package plantuml-mode)

(provide 'init-languages)
