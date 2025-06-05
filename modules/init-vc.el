;;; -*- lexical-binding: t -*-

(+when-idle! 5.0 (require 'magit))

(setopt vc-make-backup-files nil
        vc-handled-backends ()
        diff-font-lock-syntax 'hunk-only
        ediff-shell (+os/per-system! :linux "sh"
                                     :wsl "sh")
        ediff-ignore-case t
        ediff-no-emacs-help-in-control-buffer t
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        vc-directory-exclusion-list (append (or (when (boundp 'vc-directory-exclusion-list)
                                                  vc-directory-exclusion-list)
                                                '())
                                            '("straight" "node_modules" "build")))

(use-package magit
  :init
  (setq forge-add-default-bindings t
        magit-git-executable (+os/per-system! :wsl "git"
                                              :linux "git"
                                              :win "C:/Program Files/Git/cmd/git.exe"))
  :hook
  (magit-process-find-password-functions . magit-process-password-auth-source)
  :bind*
  ( :map meow-vc-global-map
    ("g" . magit)))

(use-package magit-todos
  :bind*
  ( :map meow-vc-global-map
    ("T" . magit-todos-list)))
;; :hook
;; (after-init-hook . magit-todos-mode))

(use-package forge)

(use-package consult-gh
  :straight (consult-gh :type git
                        :host github
                        :repo "armindarvish/consult-gh"))

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
  (global-diff-hl-mode-hook . diff-hl-margin-mode)
  (after-init-hook . global-diff-hl-mode)
  :bind-keymap*
  ("C-c g" . diff-hl-command-map)
  :bind*
  ( :repeat-map diff-hl-command-map
    ("r" . diff-hl-set-reference-revision)
    ("R" . diff-hl-reset-reference-revision)
    ("a" . diff-hl-amend-mode)
    ("U" . diff-hl-unstage-file)
    ("s" . diff-hl-stage-current-hunk)))


(provide 'init-vc)
