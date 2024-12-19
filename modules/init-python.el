;;; -*- lexical-binding: t -*-

(use-package anaconda-mode)

(use-package lsp-pyright
  :init
  (setq lsp-pyright-auto-import-completions t))

(use-package python-ts-mode
  :straight nil
  :init
  (setq python-indent-offset 4))

(use-package pyvenv)

(provide 'init-python)
