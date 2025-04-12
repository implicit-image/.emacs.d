;;; -*- lexical-binding: t -*-

(use-package cape
  :init
  (defun +corfu/cape-setup ()
    (require 'cape)
    (add-hook 'completion-at-point-functions 'cape-file)
    (add-hook 'completion-at-point-functions (cape-company-to-capf 'company-dabbrev-code)))

  :hook
  (corfu-mode . +corfu/cape-setup)
  (lsp-bridge-mode . +corfu/cape-setup))

(provide 'init-cape)
