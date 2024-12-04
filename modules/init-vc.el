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
  :demand
  :init
  (setq diff-hl-show-hunk-function 'diff-hl-show-hunk-inline-popup
	diff-hl-draw-borders t
	diff-hl-reference-revision "HEAD^")
  :config
  (global-diff-hl-mode)
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
