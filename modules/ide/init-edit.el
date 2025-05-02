;;; -*- lexical-binding: t -*-

(use-package evil-mc
  :init
  (setq evil-mc-undo-cursors-on-keyboard-quit t)
  :hook
  (after-init-hook . (lambda ()
                       (interactive)
                       (global-evil-mc-mode 1)
                       (add-to-list 'evil-mc-incompatible-minor-modes 'lispy-mode))))

(use-package evil-nerd-commenter
  :general
  (global-map
   :states '(visual normal)
   "g c" 'evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :hook
  (after-init-hook . global-evil-surround-mode))

(use-package vundo
  :config
  (vundo-popup-mode 1)
  :general
  (global-map
   "C-x u" '("Visualize undo" . vundo)))

(use-package paren
  :straight nil
  :after evil
  :init
  (setq show-paren-style 'parenthesis
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t))

(use-package drag-stuff
  :general
  (global-map
   :states 'visual
   "M-k" 'drag-stuff-up
   "M-j" 'drag-stuff-down))

(use-package origami
  :straight (origami :type git
                     :host github
                     :repo "elp-revive/origami.el")
  :hook
  (after-init-hook . global-origami-mode))

(use-package tabify
  :straight nil
  :general
  (+leader-keys
    "t TAB" 'tabify
    "t <tab>" 'tabify
    "t S-TAB" 'untabify
    "t <backtab>" 'untabify))

(use-package combobulate
  :straight (combobulate :type git
                         :host github
                         :nonrecursive t
                         :repo "mickeynp/combobulate")
  :functions
  (combobulate-mode)
  :custom
  (combobulate-key-prefix "C-c o")
  :hook
  (prog-mode-hook . combobulate-mode))

(use-package vlf
  :demand)

(provide 'init-edit)
