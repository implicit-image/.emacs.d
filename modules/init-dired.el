;;; -*- lexical-binding: t -*-

;;;###autoload
(defun +dired/choose-sort ()
  "Change sort type in current dired buffer."
  (interactive)
  (let* ((sort-by (completing-read "Sort by:" '("date" "name" "size" "dir")))
         (ls-args (cl-case sort-by
                    ("name" "-Al ")
                    ("date" "-Al -t")
                    ("size" "-Al -S")
                    ("dir" "-Al --group-directories-first")
                    (t "-Al"))))
    (dired-sort-other ls-args)))

(use-package dired
  :straight nil
  :init
  (defun +dired-revert-buffer-p (dir)
    (not (and (+os/is-wsl-p)
              (string-prefix-p "/mnt" dir)))) ;; dont revert windows dirs when on wsl

  (setq dired-dwim-target t
        dired-listing-switches "-alh --group-directories-first"
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-kill-when-opening-new-dired-buffer t
        dired-auto-revert-buffer t
        ;; dired-auto-revert-buffer '+dired-revert-buffer-p
        dired-create-destination-dirs 'ask)
  :hook (dired-mode-hook . (lambda ()
                             (interactive)
                             (toggle-truncate-lines +1)
                             (setq-local case-fold-search nil))))

(use-package wdired
  :straight nil
  :init
  (setq wdired-allow-to-change-permissions t))

(use-package diredfl
  :commands
  (diredfl-mode)
  :hook
  (dired-mode-hook . diredfl-mode))

(provide 'init-dired)
