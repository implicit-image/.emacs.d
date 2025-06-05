;;;###autoload
(defun +project--git-root-finder (dir)
  "Integrate .git project roots."
  (let ((dotgit (and (setq dir (locate-dominating-file dir ".git"))
                     (expand-file-name dir))))
    (and dotgit
         (cons 'transient (file-name-directory dotgit)))))

;;;###autoload
(defun +project-list-buffers-consult (_project _files-only)
  (interactive)
  (consult-project-buffer))

(provide 'init-project-impl)
