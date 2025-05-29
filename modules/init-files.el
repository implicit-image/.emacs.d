;;; -*- lexical-binding: t -*-

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

(defun +rename-visited-file ()
  (interactive)
  (rename-visited-file (expand-file-name
                        (read-file-name "Rename visited file to: "
                                        default-directory
                                        nil
                                        nil
                                        (file-name-nondirectory (buffer-file-name))))))

;; (use-package files
;;   :straight nil
;;   :init
(add-to-list 'save-some-buffers-action-alist
             (list "d"
                   (lambda (buffer)
                     (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))
;; :bind*
;; (("C-x SPC ." . find-file)
;;  ("C-x SPC b r" . revert-buffer)
;;  ("C-x SPC f o" . find-file-other-window)
;;  ("C-x SPC f R" . +rename-visited-file)
;;  ("C-x SPC f Y" . +yank-current-path)
;;  ("C-x SPC f y" . +yank-current-file)
;;  ("C-x SPC h l" . load-library)
;;  ("C-x SPC q A" . save-buffers-kill-emacs)
;;  ("C-x SPC q r" . restart-emacs)))

;; (use-package autorevert
;;   :straight nil
;;   :hook
;;   ((dired-mode-hook pdf-mode-hook) . auto-revert-mode))

(add-hook 'pdf-mode-hook 'auto-revert-mode)

(use-package ready-player
  :init
  (setq ready-player-set-global-bindings nil)
  :hook
  (after-init-hook . ready-player-mode))

(provide 'init-files)
