;;; -*- lexical-binding: t -*-

(use-package anaconda-mode)

(use-package lsp-pyright
  :init
  (setq lsp-pyright-auto-import-completions t))

(use-package python
  :init
  (setq python-indent-offset 4)
  :straight nil)

(provide 'init-python)
