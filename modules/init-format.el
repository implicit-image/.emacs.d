;;; -*- lexical-binding: t -*-

(use-package editorconfig
  :hook
  (after-init-hook . editorconfig-mode))

(use-package sqlformat)

(use-package apheleia
  :hook
  (after-init-hook . apheleia-global-mode))

(provide 'init-format)
