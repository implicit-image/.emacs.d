(use-package files
  :straight nil
  :init
  (defun +yank-current-file ()
    "Yank the name of the current file."
    (interactive)
    (let ((name (buffer-file-name (current-buffer))))
      (when name
        (kill-new (file-name-nondirectory name)))))

  (defun +yank-current-path ()
    "Yank full path of current file."
    (interactive)
      (let ((name (buffer-file-name (current-buffer))))
        (when name
          (kill-new name))))

  :general
  (+leader-keys
    "." '("Find file in cwd" . find-file)
    "b r" '("Revert" . revert-buffer)
    "f o" '("Find file in other window" . find-file-other-window)
    "f R" '("Rename current file" . rename-visited-file)
    "f y" '("Yank current file name" . +yank-current-file)
    "f Y" '("Yank current full path" . +yank-current-path)
    "h l" '("Load library" . load-library)
    "q A" '("Save all and kill emacs" . save-buffers-kill-emacs)))

(use-package autorevert
  :straight nil
  :hook
  (dired-mode . auto-revert-mode))

(use-package ready-player
  :init
  (setq ready-player-set-global-bindings nil)
  :hook
  (after-init . ready-player-mode))

(provide 'init-files)
