;;; -*- lexical-binding: t -*-

;; (use-package project
;;   :disabled
;;   :straight nil)

(use-package projectile
  :init
  (setq projectile-keymap-prefix "C-c p")

  (defun +projects/revert-buffers (&optional choose-project)
    "Reverts all buffers asociated wit current project. If CHOOSE-PROJECT is t\
query for known project and revert its buffers instead."
    (interactive)
    (let ((project (if choose-project
                       (completing-read "Revert buffers:"
                                        projectile-known-projects)
                     (projectile-project-root))))
      (dolist (buff (projectile-project-buffers project))
        (if (buffer-file-name buff)
            (revert-buffer-quick buff)))))

  :config
  (projectile-mode +1))

(use-package direnv
  :if (+os/is-linux-p)
  :init
  (setq direnv-always-show-summary nil
        direnv-show-paths-in-summary nil)
  :hook
  (after-init-hook . direnv-mode))

(provide 'init-projects)
