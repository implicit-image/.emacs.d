;;; -*- lexical-binding: t -*-

;;;###autoload
(defun +dired/choose-sort ()
  "Change sort type in current dired buffer."
  (interactive)
  (let* ((sort-by (completing-read "Sort by:" '("date" "name" "size" "dir")))
         (ls-args (cl-case sort-by
                    (("name") "-Al ")
                    (("date") "-Al -t")
                    (("size") "-Al -S")
                    (("dir") "-Al --group-directories-first")
                    (t "-Al"))))
    (dired-sort-other ls-args)))

(use-package dired
  :straight nil
  :init
  (setq dired-dwim-target t
        dired-listing-switches "-alh"
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-auto-revert-buffer t
        dired-create-destination-dirs 'ask)
  :hook ((dired-mode) . (lambda ()
                          (interactive)
                          (toggle-truncate-lines +1)
                          (setq-local case-fold-search nil))))

(use-package diredfl
  :commands
  (diredfl-mode)
  :hook
  (dired-mode . diredfl-mode))

(provide 'init-dired)
