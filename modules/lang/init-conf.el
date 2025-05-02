;;; -*- lexical-binding: t -*-
(use-package toml-mode)

(use-package nxml-mode
  :straight nil
  :hook
  (nxml-mode-hook . +whitespace-off))

(use-package nix-mode)

(provide 'init-conf)
