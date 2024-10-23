
(use-package anaconda-mode)

(use-package lsp-pyright)

(use-package python
  :init
  (setq python-indent-offset 4)
  :straight nil)

(provide 'implicit-python)
