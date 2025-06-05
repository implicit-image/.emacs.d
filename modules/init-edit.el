;;; -*- lexical-binding: t -*-

(use-package avy
  :bind*
  ( :map meow-jump-global-map
    ("s" . avy-goto-symbol-1)
    ("w" . avy-goto-word-1)
    ("l" . avy-goto-line)))

(use-package vundo
  :custom
  (vundo-window-max-height 6)
  :bind*
  (("C-/" . vundo)
   :map vundo-mode-map
   ("r" . vundo-forward)
   ("q" . vundo-confirm)
   ("u" . vundo-backward)
   ("C-/" . vundo-backward)
   ("C-?" . vundo-forward)))

(setopt show-paren-style 'mixed
        show-paren-delay 0.05
        show-paren-context-when-offscreen 'overlay
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)



;; use the custom function instead of the provided one
(advice-add 'show-paren--show-context-in-overlay :override '+paren--overlay-function)

(add-hook 'prog-mode-hook 'show-paren-local-mode)

(use-package drag-stuff
  :bind
  (("M-k" . drag-stuff-up)
   ("M-j" . drag-stuff-down))
  :bind*
  ( :map prog-mode-map
    ("M-k" . drag-stuff-up)
    ("M-j" . drag-stuff-down)))

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

(use-package multiple-cursors
  :init
  (setq mc/always-run-for-all t)

  (setq mc/cmds-to-run-once (append (when (boundp 'mc/cmds-to-run-once)
                                      mc/cmds-to-run-once)
                                    '()))
  :bind-keymap
  ("C-x |" . meow-mc-global-map)
  :bind*
  ( :repeat-map meow-mc-global-map
    ("l" . mc/edit-beginnings-of-lines)
    ("L" . mc/edit-ends-of-lines)
    ("w" . mc/mark-all-words-like-this)
    ("n" . mc/insert-numbers)
    ("W" . mc/mark-all-symbols-like-this)
    ("}" . mc/mark-next-like-this)
    ("{" . mc/mark-previous-like-this)
    ("v" . mc/vertical-align)
    :exit
    ("i" . meow-insert)))

(use-package vlf)

(setq treesit-font-lock-level 3
      set-mark-command-repeat-pop t
      backward-delete-char-untabify-method 'hungry)

(use-package treesit-auto
  :commands
  (global-treesit-auto-mode)
  :init
  (setq treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  :hook
  (after-init-hook . global-treesit-auto-mode))

(provide 'init-edit)
