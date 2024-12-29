;;; -*- lexical-binding: t -*-

(use-package auctex)

(use-package cdlatex
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  (org-mode . org-cdlatex-mode))

(use-package xenops)

(provide 'init-latex)
