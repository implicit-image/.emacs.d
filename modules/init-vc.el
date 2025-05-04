;;; -*- lexical-binding: t -*-

(use-package magit
  :init
  (setq forge-add-default-bindings nil)
  :hook
  (magit-process-find-password-functions . magit-process-password-auth-source)
  :general
  (+leader-keys
    "g g" '("Magit" . magit)
    "p t" '("List project todos" . magit-todos-list)))

(use-package magit-todos)

(use-package forge
  :init
  (with-eval-after-load 'magit
    (require 'forge)))

(use-package consult-gh
  :straight (consult-gh :type git
                        :host github
                        :repo "armindarvish/consult-gh")
  :general
  (+leader-keys
    "g i" '(:ignore t :which-key "[I]ssue")
    "g i l" '("List issues" . consult-gh-issue-list)
    "g i c" '("Create issue" . consult-gh-issue-create)
    "g i d" '("Delete issue" . consult-gh-issue-delete)
    "g r" '(:ignore t :which-key "[R]epo")
    "g r c" '("Clone repo" . consult-gh-repo-clone)
    "g r C" '("Create repo" . consult-gh-repo-create)
    "g r f" '("Fork repo" . consult-gh-repo-fork)
    "g s" '(:ignore t :which-key "[S]earch")
    "g s p" '("Search PRs" . consult-gh-search-prs)
    "g s c" '("Search code" . consult-gh-search-code)
    "g s r" '("Search repos" . consult-gh-search-repos)))

(use-package vdiff)

(use-package vc
  :straight nil
  :init
  (setq vc-make-backup-files nil))


(use-package diff-hl
  :init
  (setq diff-hl-show-hunk-function 'diff-hl-show-hunk-inline-popup
        diff-hl-draw-borders t
        diff-hl-update-async t
        diff-hl-margin-symbols-alist '((insert . "+")
                                       (delete . "-")
                                       (change . "=")
                                       (unknown . "?")
                                       (ignored . "i"))
        diff-hl-reference-revision nil)
  :hook
  (dired-mode-hook . diff-hl-dired-mode)
  (after-init-hook . (lambda ()
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

(use-package blamer
  :custom-face
  (blamer-face ((t (:slant normal))))
  (blamer-pretty-border-face ((t (:foreground ,(doom-color 'yellow)))))
  :custom
  (blamer-idle-time 10)
  (blamer-min-offset 150)
  (blamer-view 'overlay-right)
  :init
  (setq blamer-type 'margin-overlay
        blamer-commit-formatter "❌ %s"
        blamer--overlay-popup-position 'smart)
  :general
  (+mode-keys
    :keymaps 'override
    :states '(normal visual)
    "g b" 'blamer-show-commit-info))

(provide 'init-vc)
