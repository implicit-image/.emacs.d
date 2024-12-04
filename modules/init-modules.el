;;; -*- lexical-binding: t -*-

(defun +modules/browse ()
  "Open user init module file."
  (interactive)
  (require 'f)
  (ivy-read
   "Open init module file: "
    (-filter (lambda (path)
	      (string-suffix-p ".el" path))
	     (directory-files +modules/path))
   :preselect 0
   :require-match t
   :action (lambda (name)
	     (find-file (f-join +modules/path name)))))

(+leader-keys
  "f P" '("Browse modules" . +modules/browse))

(provide 'init-modules)
