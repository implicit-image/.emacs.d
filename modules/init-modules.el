;;; -*- lexical-binding: t -*-

(defun +modules/browse ()
  "Open user init module file."
  (interactive)
  (require 'f)
  (consult--read (-filter (lambda (path)
			    (string-suffix-p ".el" path))
			  (directory-files +modules/path))
		 :prompt "Open init module file: "
		 :require-match t
		 :lookup (lambda (name &rest args)
			   (find-file (f-join +modules/path name)))))

(+leader-keys
  "f P" '("Browse modules" . +modules/browse))

(provide 'init-modules)
