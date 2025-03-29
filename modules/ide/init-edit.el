;;; -*- lexical-binding: t -*-

(use-package evil-mc
  :hook
  (after-init . (lambda ()
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
  (after-init . global-evil-surround-mode))

(use-package vundo
  :config
  (+windows-cfg '(()))
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

(use-package origami
  :straight (origami :type git
                     :host github
                     :repo "elp-revive/origami.el")
  :hook
  (after-init . global-origami-mode))

(use-package symbol-overlay
  :custom-face
  (symbol-overlay-default-face ((t (:box (:line-width -1)))))
  :custom
  (symbol-overlay-idle-timer 0.3)
  :config
  :hook
  ((prog-mode) . symbol-overlay-mode))

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
  (prog-mode . combobulate-mode))

(use-package vlf
  :demand)

(provide 'init-edit)
