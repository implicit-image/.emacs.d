;;; -*- lexical-binding: t -*-


(use-package server
  :straight nil
  :init
  (defun +server-after-frame ()
    (interactive))
  :hook
  (server-after-frame-make-hook . +server-after-frame)
