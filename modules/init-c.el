(use-package cc-mode)

(use-package c-ts-mode
  :mode "\\.c\\'"
  :init
  (setq c-ts-mode-indent-offset 4))

(use-package rtags)

(use-package ccls)

(provide 'init-c)
