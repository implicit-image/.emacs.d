;;; -*- lexical-binding: t -*-

(use-package grep
  :straight nil
  :init
  (setq grep-program "rg")

  ;; for preview in grep mode
  (defun +next-error-no-select (&optional n)
    (interactive "p")
    (save-selected-window
      (let ((next-error-highlight next-error-highlight-no-select)
            (display-buffer-overriding-action
             '((display-buffer-reuse-window display-buffer-reuse-mode-window)
               (inhibit-same-window . t))))
        (next-error n))))

  (defun +previous-error-no-select (&optional n)
    (interactive "p")
    (+next-error-no-select (- (or n 1))))

  :general
  (grep-mode-map
   :states 'normal
   "M-n" '+next-error-no-select
   "M-p" '+previous-error-no-select))

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
        imenu-use-popup-menu 'on-mouse))

(use-package isearch
  :straight nil)

(+leader-keys
  "*" '("Find thing-at-point in project" . +search/rg-thing-at-point))

(provide 'init-search)
