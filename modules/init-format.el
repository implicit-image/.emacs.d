;;; -*- lexical-binding: t -*-

(use-package editorconfig
  :init
  (setopt editorconfig-exclude-modes '(lisp-interaction-mode))
  :hook
  (after-init-hook . editorconfig-mode))

(use-package apheleia
  :defer 5
  :hook
  (after-init-hook . apheleia-global-mode))

(provide 'init-format)
