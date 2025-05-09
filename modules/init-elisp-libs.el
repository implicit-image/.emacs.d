;;; -*- lexical-binding: t -*-

(use-package f)

(use-package dash)

(use-package async
  :autoload (dired-async-mode)
  :hook
  (after-init-hook . dired-async-mode))

(use-package ov)

(provide 'init-elisp-libs)
