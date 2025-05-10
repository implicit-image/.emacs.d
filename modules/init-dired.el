;;; -*- lexical-binding: t -*-

(use-package dired
  :straight nil
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alh --group-directories-first")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-kill-when-opening-new-dired-buffer t)
  (ired-auto-revert-buffer t)
  ;; dired-auto-revert-buffer '+dired-revert-buffer-p
  (dired-create-destination-dirs 'ask)
  :hook (dired-mode-hook . (lambda ()
                             (interactive)
                             (toggle-truncate-lines +1)
                             (setq-local case-fold-search nil))))

(use-package wdired
  :straight nil
  :custom
  (wdired-allow-to-change-permissions t))

(use-package diredfl
  :commands
  (diredfl-mode)
  :hook
  (dired-mode-hook . diredfl-mode))

(provide 'init-dired)
