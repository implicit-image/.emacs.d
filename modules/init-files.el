(use-package files
  :straight nil
  :general
  (+leader-keys
    "." '("Find file in cwd" . find-file)
    "b r" '("Revert" . revert-buffer)
    "f o" '("Find file in other window" . find-file-other-window)
    "f R" '("Rename current file" . rename-visited-file)
    "h l" '("Load library" . load-library)
    "q A" '("Save all and kill emacs" . save-buffers-kill-emacs)))

(use-package ready-player
  :hook
  (after-init . ready-player-mode))

(provide 'init-files)
