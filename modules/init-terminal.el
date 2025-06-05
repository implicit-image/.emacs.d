;;; -*- lexical-binding: t -*-

(use-package vterm
  :init
  (+set-tab-function! vterm-mode vterm-send-tab)
  (with-eval-after-load 'exec-path-from-shell
    (setopt vterm-shell (exec-path-from-shell-getenv "SHELL")))
  :bind*
  ( :map meow-special-global-map
    ("v" . vterm))
  :hook
  (vterm-mode-hook . hl-line-mode))

(use-package eat
  :bind*
  ( :map meow-special-global-map
    ("e" . eat)
    :map project-prefix-map
    ("E" . eat-project)))

(provide 'init-terminal)
