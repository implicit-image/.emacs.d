
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
  :after evil
  :init
  (add-hook
   'evil-mc-before-cursors-created
   '+editing/evil-mc-turn-off-incompatible-modes)
  (add-hook
   'evil-mc-after-cursors-deleted
   '+editing/evil-mc-turn-on-incompatible-modes)
  :config
  (global-evil-mc-mode 1))


(use-package evil-nerd-commenter
  :after evil)

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
				       (display-line-numbers-mode -1))))

(use-package paren
  :straight nil
  :after evil
  :custom-face
  (show-paren-match ((t (:foreground unspecified))))
  (show-paren-match-expression ((t (:box
				    (:line-width -1 :color ,(doom-color 'base5) :style released-button)
				    :foreground
				    unspecified))))
  :init
  (setq show-paren-style 'expression
	show-paren-when-point-inside-paren t
	show-paren-when-point-in-periphery t))

(provide 'implicit-editing)
