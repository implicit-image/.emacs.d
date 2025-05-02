;;; -*- lexical-binding: t -*-

(use-package auctex)

(use-package cdlatex
  :hook
  (LaTeX-mode-hook . turn-on-cdlatex)
  (org-mode-hook . org-cdlatex-mode))

(use-package xenops)

(provide 'init-latex)
