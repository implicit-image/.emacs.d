;;; -*- lexical-binding: t -*-

(use-package f)

(use-package dash)

(use-package async
  :autoload (dired-async-mode))

(with-eval-after-load 'dired
  (dired-async-mode 1))

(use-package ov)

(provide 'init-elisp-libs)
