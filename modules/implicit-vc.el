


(use-package magit)

(use-package magit-todos)

(use-package vdiff)

(use-package vc
  :straight nil
  :init
  (+windows/cfg
   '(("\*vc-diff\**")
     :regexp t :height 0.5 :position bottom :dedicated nil)))

(use-package diff-hl
  :demand
  :init
  (setq diff-hl-show-hunk-function 'diff-hl-show-hunk-inline-popup
	diff-hl-draw-borders t
	diff-hl-reference-revision "HEAD^")
  :config
  (global-diff-hl-mode))


(provide 'implicit-vc)
