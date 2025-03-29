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
  (after-init . (lambda ()
                  (interactive)
                  (evil-collection-init)
                  ;; (evil-collection-init evil-collection-mode-list)
                  )))

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
  ((org-mode) . evil-org-mode)
  ((org-agenda) . (lambda ()
                    (require 'evil-org-agenda)
                    (evil-org-agenda-set-keys))))

;;;; evil bindings for treemacs
(use-package treemacs-evil
  :demand
  :after treemacs
  :general
  (evil-treemacs-state-map
   :states '(motion)
   "p" 'treemacs-project-map))

(provide 'init-evil)
