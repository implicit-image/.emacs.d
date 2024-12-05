;;; -*- lexical-binding: t -*-

(defvar +editing/evil-mc-incompatible-modes '()
  "Modes not compatible with `evil-mc'.")


(defvar-local +editing/evil-mc-disabled-modes '()
  "Temporarily disabled modes in current buffer.")


(defvar +editing/input-methods '()
  "Input methods to cycle betweem using `+editing/cycle-input-method'.")

(defvar-local +editing--current-method-index 0
  "Index of currently selected input method.")

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

(defun +editing/cycle-input-method (&optional arg)
  (interactive "p")
  (let* ((arg (+ (or arg 1) +editing--current-method-index))
	 (method (+utils-nth-wrapped arg +editing/input-methods)))
    (set-input-method method)))

(setq +editing/input-methods '())

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

(use-package editorconfig
  :demand
  :config
  (editorconfig-mode 1))

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
  :hook (undo-tree-visualizer-mode . (lambda ()
				       (display-line-numbers-mode -1)))
  :general
  (+leader-keys
    "o u" '("Undo tree" . undo-tree-visualize)))

(use-package paren
  :straight nil
  :after evil
  :custom-face
  (show-paren-match ((t (:background ,(doom-color 'red) :foreground unspecified))))
  (show-paren-match-expression ((t (:box
				    (:line-width -1 :color ,(doom-color 'base6) :style released-button)
				    :foreground
				    unspecified))))
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

;; (use-package combobulate
;;   :straight (combobulate :type git
;; 			 :host github
;; 			 :repo "mickeynp/combobulate")
;;   :custom
;;   (combobulate-key-prefix "C-c o"))

(provide 'init-editing)
