;;; -*- lexical-binding: t -*-

;; TODO: complete dotnet config
(use-package csharp-mode
  :straight nil
  :mode "\\.cs\\'")

(use-package csproj-mode)

(use-package sln-mode)

(use-package shader-mode)

(use-package sharper)

(use-package fsharp-mode
  :mode "\\.fs\\'")

(use-package lsp-fsharp
  :straight nil)

(use-package fstar-mode)

(provide 'init-dotnet)
