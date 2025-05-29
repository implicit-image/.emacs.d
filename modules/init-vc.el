;;; -*- lexical-binding: t -*-

(use-package magit
  :init
  (setq forge-add-default-bindings nil
        magit-git-executable (+os/per-system! :wsl "git"
                                              :linux "git"
                                              :win "C:/Program Files/Git/cmd/git.exe"))

  :hook
  (magit-process-find-password-functions . magit-process-password-auth-source))
;; :bind*
;; (("C-x <space> g g" . magit)
;;  ("C-x <space> p t" . magit-todos-alist)))

(use-package magit-todos)

(use-package forge
  :init
  (with-eval-after-load 'magit
    (require 'forge)))

(use-package consult-gh
  :straight (consult-gh :type git
                        :host github
                        :repo "armindarvish/consult-gh"))
;; :bind*
;; ( :map global-override-map
;;   ("C-x <space> g i l" . consult-gh-issue-list)
;;   ("C-x <space> g i c" . consult-gh-issue-create)
;;   ("C-x <space> g i d" . consult-gh-issue-delete)
;;   ("C-x <space> g r c" . consult-gh-repo-clone)
;;   ("C-x <space> g r C" . consult-gh-repo-create)
;;   ("C-x <space> g r f" . consult-gh-repo-fork)
;;   ("C-x <space> g s p" . consult-gh-search-prs)
;;   ("C-x <space> g s c" . consult-gh-search-code)
;;   ("C-x <space> g s r" . consult-gh-search-repos)))

(use-package vdiff)

;; (use-package vc
;;   :straight nil
;;   :init
;;   (setq vc-make-backup-files nil))

(setopt vc-make-backup-files nil)


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
                       (global-diff-hl-mode +1))))
;; :bind-keymap*
;; ("C-x <space> g v" . diff-hl-command-map))

(use-package blamer
  :custom-face
  (blamer-face ((t (:slant normal))))
  (blamer-pretty-border-face ((t (:foreground ,(doom-color 'yellow)))))
  :custom
  (blamer-idle-time 0.1)
  (blamer-min-offset 150)
  (blamer-view 'overlay)
  :init
  (setq blamer-type 'both
        blamer-commit-formatter "❌ %s"
        blamer--overlay-popup-position 'bottom))

(provide 'init-vc)
