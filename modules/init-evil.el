;;; -*- lexical-binding: t -*-

;;;; packages for evil emulation


(use-package evil
  :demand
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-undo-system 'undo-redo
        evil-lookup-func 'woman)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'calibredb-search-mode 'normal)
  (evil-set-initial-state 'calibredb-show-mode 'normal)
  (evil-set-initial-state 'pdf-view-mode 'normal)
  (evil-set-initial-state 'org-agenda-mode 'normal)
  (evil-set-initial-state 'vterm-mode 'insert)
  (evil-set-initial-state 'eat-mode 'insert)
  (evil-set-initial-state 'Info-mode 'normal)
  (evil-set-initial-state 'shell-command-mode 'normal)
  (evil-set-initial-state 'comint-mode 'normal)
  (evil-set-initial-state 'profiler-report-mode 'normal)
  :general
  (+leader-keys
    "`" '("Last buffer" . evil-switch-to-windows-last-buffer)
    "c d" '("Goto definition" . evil-goto-definition)
    "w s" '("Split window horizontally" . evil-window-split)
    "w v" '("Split window vertically" . evil-window-vsplit))
  (general-override-mode-map
   :states '(normal visual)
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down
   "C-k" 'evil-window-up
   "C-l" 'evil-window-right)
  (global-map
   "C-TAB" 'evil-jump-forward
   "C-<tab>" 'evil-jump-forward))

;;;; collection of evil bindings for popular modes
(use-package evil-collection
  :hook
  (after-init-hook . (lambda ()
                       (interactive)
                       (evil-collection-init))))

;;;; highlight the AOE of vim motions
(use-package evil-goggles
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package vimish-fold
  :after evil)

;;;; vim-like code folding
(use-package evil-vimish-fold
  :after vimish-fold
  :init
  (setq evil-vimish-fold-target-modes '(prog-mode markdown-mode conf-mode text-mode))
  :config
  (global-evil-vimish-fold-mode 1))

;;;; evil bindings for org-mode
(use-package evil-org
  :config
  (evil-org-set-key-theme)
  :hook
  (org-mode-hook . evil-org-mode)
  (org-agenda-hook . (lambda ()
                       (require 'evil-org-agenda)
                       (evil-org-agenda-set-keys))))

;;;; evil bindings for treemacs
(use-package treemacs-evil
  :init
  (with-eval-after-load 'treemacs
    (require 'treemacs-evil))
  :general
  (evil-treemacs-state-map
   "SPC w" 'evil-window-next
   "C-w" 'evil-window-next
   "C-l" 'evil-window-right
   "C-k" 'evil-window-up
   "C-p" '(:keymap projectile-command-map :package projectile)
   "C-h" 'evil-window-left
   "C-j" 'evil-window-down))

(use-package evil-textobj-tree-sitter
  :after (treeesit evil))

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

(use-package evil-anzu
  :init
  (setq anzu-search-threshold nil
        anzu-replace-threshold nil)
  :hook
  (after-init-hook . global-anzu-mode)
  :general
  (+leader-keys
    "c R" '("Rename" . anzu-query-replace)
    "s r" '("Query replace" . anzu-query-replace)
    "s R" '("Replace at point" . anzu-replace-threshold)
    "s R" '("Query replace regexp" . anzu-query-replace-regexp)))

(provide 'init-evil)
