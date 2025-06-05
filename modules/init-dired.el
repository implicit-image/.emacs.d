;;; -*- lexical-binding: t -*-

(use-package dired
  :straight nil
  :init
  (setopt dired-dwim-target t
          dired-listing-switches "-alh --group-directories-first"
          dired-recursive-copies 'always
          dired-recursive-deletes 'top
          dired-kill-when-opening-new-dired-buffer nil
          dired-auto-revert-buffer t
          dired-create-destination-dirs 'ask
          wdired-allow-to-change-permissions t
          wdired-use-dired-vertical-movement 'sometimes))

(add-hook 'dired-mode-hook '+dired-mode--setup)

(bind-keys*
 ("M-g M-d" . dired-at-point)
 :map dired-mode-map
 ("C-e" . wdired-change-to-wdired-mode)
 ("i" . wdired-change-to-wdired-mode)
 ("-" . dired-up-directory))

(use-package diredfl
  :commands
  (diredfl-mode)
  :hook
  (dired-mode-hook . diredfl-mode))

(provide 'init-dired)
