
(use-package dap-mode
  :config (dap-auto-configure-mode))

(use-package dape
  :init
  (setq dape-key-prefix "C-x D")
  :config
  (setq dape-buffer-window-arrangement 'right))

(provide 'init-debug)
