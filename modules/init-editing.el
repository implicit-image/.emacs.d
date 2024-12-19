;;; -*- lexical-binding: t -*-

(defvar +editing/evil-mc-incompatible-modes '()
  "Modes not compatible with `evil-mc'.")


(defvar-local +editing/evil-mc-disabled-modes '()
  "Temporarily disabled modes in current buffer.")

(defun +editing/evil-mc-turn-off-incompatible-modes ()
  (interactive)
  (mapc (lambda (mode-symbol)
	  (when (symbol-value mode-symbol)
	    (progn
	      ;; disable enabled mode
	      (funcall-interactively mode-symbol -1)
	      ;;  save what was disabled
	      (add-to-list '+editing/evil-mc-disabled-modes mode-symbol))))
	+editing/evil-mc-incompatible-modes))

(defun +editing/evil-mc-turn-on-incompatible-modes ()
  (interactive)
  (mapc (lambda (mode-symbol)
	  (funcall-interactively mode-symbol +1))
	+editing/evil-mc-disabled-modes))

(use-package evil-mc
  :init
  (add-hook
   'evil-mc-before-cursors-created
   '+editing/evil-mc-turn-off-incompatible-modes)
  (add-hook
   'evil-mc-after-cursors-deleted
   '+editing/evil-mc-turn-on-incompatible-modes)
  :hook
  (after-init . (lambda ()
		  (interactive)
		  (global-evil-mc-mode 1))))

(use-package evil-nerd-commenter
  :general
  (global-map
   :states '(visual normal)
   "g c" 'evilnc-comment-or-uncomment-lines))

(use-package evil-surround
  :hook
  (after-init . global-evil-surround-mode))

(use-package editorconfig
  :hook
  (after-init . editorconfig-mode))

(use-package undo-tree
  :demand
  :init
  (require 'f)
  (setq undo-tree-history-directory-alist `(("." . ,(f-join user-emacs-directory "undo-tree/"))))
  (+windows-cfg
   '((" *undo-tree*")
     :width 0.2 :position right))
  :config
  (global-undo-tree-mode +1)
  :general
  (+leader-keys
    "o u" '("Undo tree" . undo-tree-visualize)))

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
  :custom
  (symbol-overlay-idle-timer 0.3)
  :config
  :hook
  ((prog-mode) . symbol-overlay-mode))

(use-package combobulate
  :straight (combobulate :type git
			 :host github
			 :repo "mickeynp/combobulate")
  :functions
  (combobulate-mode)
  :custom
  (combobulate-key-prefix "C-c o"))

(use-package apheleia
  :hook
  (after-init . apheleia-global-mode))

(provide 'init-editing)
