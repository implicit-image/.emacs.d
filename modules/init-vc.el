;;; -*- lexical-binding: t -*-

(use-package magit
  :general
  (+leader-keys
    "g g" '("Magit" . magit)
    "p t" '("List project todos" . magit-todos-list)))

(use-package magit-todos)

(use-package vdiff)

(use-package vc
  :straight nil
  :init
  (+windows-cfg
   '(("\*vc-diff\**")
     :regexp t :height 0.5 :position bottom :dedicated nil)))

(use-package diff-hl
  :init
  (setq diff-hl-show-hunk-function 'diff-hl-show-hunk-inline-popup
	diff-hl-draw-borders t
	diff-hl-margin-symbols-alist '((insert . "+")
				       (delete . "-")
				       (change . "=")
				       (unknown . "?")
				       (ignored . "i"))
	diff-hl-reference-revision "HEAD^")
  :hook
  (dired-mode . diff-hl-dired-mode)
  (after-init . (lambda ()
		  (interactive)
		  (diff-hl-margin-mode +1)
		  (global-diff-hl-mode +1)))
  :general
  (diff-hl-mode-map
   :states '(normal visual)
   :prefix "SPC g"
   :global-prefix "M-SPC g"
   "v" 'diff-hl-command-map)
  (diff-hl-show-hunk-map
   :states '(normal visual)
   "]" 'diff-hl-show-hunk-posframe))

(provide 'init-vc)
