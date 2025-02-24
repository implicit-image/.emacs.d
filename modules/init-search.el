;;; -*- lexical-binding: t -*-

(use-package rg)

(defun +search/rg-thing-at-point ()
  (interactive)
  (let ((s (symbol-at-point)))
    (consult-ripgrep
     (cond ((bound-and-true-p projectile-project-root) (projectile-project-root))
           ((bound-and-true-p project-root) (project-root))
           (t default-directory))
     (if (eq s nil) "" (symbol-name s)))))

(use-package imenu
  :straight nil
  :config
  (setq imenu-auto-rescan t
        imenu-use-popup-menu t))

(use-package isearch
  :straight nil)

(+leader-keys
  "*" '("Find thing-at-point in project" . +search/rg-thing-at-point))

(provide 'init-search)
