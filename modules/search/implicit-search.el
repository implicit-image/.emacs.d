;;; -*- lexical-binding: t -*-
(require 'affe)
(require 'project)

(defun +search/rg-thing-at-point ()
  (interactive)
  (let ((s (symbol-at-point)))
    (consult-ripgrep
     (or (project-root (project-current))
         default-directory)
     (if (eq s nil) "" (symbol-name s)))))

(defun +search/affe-grep-thing-at-point (choose-dir)
  (interactive "P")
  (let ((default-directory (or (project-root (project-current))
                               default-directory))
        (s (symbol-at-point)))
    (affe-grep (if choose-dir
                   (read-directory-name "Grep in: "
                                        default-directory)
                 default-directory)
               (if (eq s nil) "" (symbol-name s)))))

(defun +search/affe-find-thing-at-point (choose-dir)
  (interactive "P")
  (let ((default-directory (or (project-root (project-current))
                               default-directory))
        (s (symbol-at-point)))
    (affe-find (if choose-dir
                   (read-directory-name "Find in: "
                                        default-directory)
                 default-directory)
               (if (eq s nil) "" (symbol-name s)))))

(defun +search/buffer ()
  "run this buffer's search function."
  (interactive)
  (command-execute +search-buffer-function))

(provide 'implicit-search)
