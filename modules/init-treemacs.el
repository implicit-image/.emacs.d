;;; -*- lexical-binding: t -*-

(use-package treemacs
  :init
  (setq treemacs-indent-guide-style 'line
        treemacs-no-png-images t
        treemacs-icons nil
        treemacs-eldoc-display 'detailed
        treemacs-file-event-delay 1000
        treemacs-file-follow-delay 0.1
        treemacs-tag-follow-delay 0.1
        treemacs-follow-after-init nil
        treemacs-expand-after-init t
        treemacs-indentation 2)

  (when (bound-and-true-p eldoc-box-hover-mode)
    (add-to-list '+eldoc-minibuffer-display-modes 'treemacs-mode))

  (add-hook 'treemacs-mode '+treemacs--setup)

  (with-eval-after-load 'treemacs
    (treemacs-tag-follow-mode 1)
    (treemacs-project-follow-mode 1)
    (treemacs-git-commit-diff-mode 1))

  (with-eval-after-load 'init-meow
    (bind-keys*
     :map meow-toggle-global-map
     ("T" . treemacs)))

  :config
  (treemacs-load-theme "Default")
  :bind*
  (("C-c C-t" . treemacs)))

(provide 'init-treemacs)
