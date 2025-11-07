;;; -*- lexical-binding: t -*-
(require 'project)

(defvar-local +local-project-root nil)

(defun +buffers-cache-local-vars ()
  "Cache local buffer information."
  (when-let ((old (not +local-project-root))
             (root (project-root (project-current))))
    (setq-local +local-project-root root)))


(provide 'implicit-buffers)
