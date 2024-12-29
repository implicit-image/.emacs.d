;;; -*- lexical-binding: t -*-

(use-package editorconfig
  :hook
  (after-init . editorconfig-mode))

(use-package sqlformat)

(use-package apheleia
  :hook
  (after-init . apheleia-global-mode))

(provide 'init-format)
