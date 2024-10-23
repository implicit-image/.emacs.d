
(use-package vterm
  :init
  (evil-set-initial-state 'vterm-mode 'insert)
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1))))



(provide 'implicit-terminal)
