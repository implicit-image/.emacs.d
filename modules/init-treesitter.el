;;; -*- lexical-binding: t -*-

(defvar +treesitter/grammar-alist '())

(defmacro +treesitter/grammar! (mode grammar-link)
  "Associate GRAMMAR-LINK with MODE."
  (let ((lang-symbol (-first-item (string-split (symbol-name mode) "-"))))
    `(progn (add-to-list +treesitter/grammar-list (,mode . (,lang-symbol ,grammar-link)))
            (add-to-list treesit-language-source-alist (,lang-symbol . (,grammar-link))))))

(defun +treesitter/install-all-grammars ()
  "Install all grammars registered in `+treesitter/grammar-alist'."
  (interactive)
  (dolist (grammar +treesitter/grammar-alist)
    (let ((url (-second-item (-second-item grammar)))
          (lang (car grammar)))
      (treesit-install-language-grammar lang))))

(setq treesit-font-lock-level 3)

(use-package treesit-auto
  :commands
  (global-treesit-auto-mode)
  :init
  (setq treesit-auto-install t)
  :hook
  (after-init-hook . (lambda ()
                  (interactive)
                  (require 'treesit-auto)
                  (treesit-auto-add-to-auto-mode-alist 'all)
                  (global-treesit-auto-mode))))


(use-package evil-textobj-tree-sitter
  :after (treeesit evil))

(provide 'init-treesitter)
