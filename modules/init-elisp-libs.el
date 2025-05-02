;;; -*- lexical-binding: t -*-

(use-package f)

(use-package dash)

(use-package async
  :config
  (autoload 'dired-async-mode "dired-async.el" nil t)
  :hook
  (after-init-hook . dired-async-mode))

(use-package ov)

(provide 'init-elisp-libs)
