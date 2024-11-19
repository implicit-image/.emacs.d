
(defvar +edit/evil-mc-incompatible-modes '()
  "Modes not compatible with `evil-mc'.")


(defvar-local +edit/evil-mc-disabled-modes '()
  "Temporarily disabled modes in current buffer.")


(defvar +edit/input-methods '()
  "Input methods to cycle betweem using `+edit/cycle-input-method'.")

(defvar-local +edit--current-method-index 0
  "Index of currently selected input method.")

(defun +edit/evil-mc-turn-off-incompatible-modes ()
  (interactive)
  (mapc (lambda (mode-symbol)
	  (when (symbol-value mode-symbol)
	    (progn
	      ;; disable enabled mode
	      (funcall-interactively mode-symbol -1)
	      ;;  save what was disabled
	      (add-to-list '+edit/evil-mc-disabled-modes mode-symbol))))
	+edit/evil-mc-incompatible-modes))

(defun +edit/evil-mc-turn-on-incompatible-modes ()
  (interactive)
  (mapc (lambda (mode-symbol)
	  (funcall-interactively mode-symbol +1))
	+edit/evil-mc-disabled-modes))

(defun +edit/cycle-input-method (&optional arg)
  (interactive "p")
  (let* ((arg (+ (or arg 1) +edit--current-method-index))
	 (method (+utils/nth-wrapped arg +edit/input-methods)))
    (set-input-method method)))

(setq +edit/input-methods '())

(use-package evil-mc
  :after evil
  :init
  (add-hook
   'evil-mc-before-cursors-created
   '+edit/evil-mc-turn-off-incompatible-modes)
  (add-hook
   'evil-mc-after-cursors-deleted
   '+edit/evil-mc-turn-on-incompatible-modes)
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
  :config
  (global-undo-tree-mode 1)
  (push '(" *undo-tree*"
	  :width 0.2
	  :position left) popwin:special-display-config))


(provide 'implicit-editing)
