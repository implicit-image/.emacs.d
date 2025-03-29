;;; -*- lexical-binding: t -*-

(use-package cape
  :init
  (defun +corfu/cape-setup ()
    (require 'cape)
    (add-hook 'completion-at-point-functions 'cape-file)
    (add-hook 'completion-at-point-functions 'cape-dabbrev))
  :hook
  (corfu-mode . +corfu/cape-setup))

(provide 'init-cape)
