;;; -*- lexical-binding: t -*-

(use-package erlang
  :init (add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode)))

(use-package otp)

(use-package edts)

(provide 'init-erlang)
