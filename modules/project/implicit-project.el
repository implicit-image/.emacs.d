;;; -*- lexical-binding: t -*-

;;;###autoload
(defun +project--root-finder (dir)
  "Integrate .git project roots."
  (let ((dotgit (and (setq dir (locate-dominating-file dir ".git"))
                     (expand-file-name dir))))
    (and dotgit
         (cons 'transient (file-name-directory dotgit)))))

(defun +project-magit-status ()
  (interactive)
  (magit-status (project-root (project-current))))

;;;###autoload
(defun +project-list-buffers-consult (_project _files-only)
  (interactive)
  (consult-project-buffer))

(defun +project-vterm (&optional new-session)
  (interactive)
  (let* ((root (or (project-name (project-current))
                   (car (last (file-name-split default-directory) 2))))
         (default-directory (project-root (project-current)))
         (vterm-buffer-name (generate-new-buffer-name (format "vterm[%s]" root))))
    (if new-session
        (vterm)
      (display-buffer))))

(defun ii/project-occur (regexp &optional all-buffers)
  "Search for lines matching REGEXP in project file buffers. If ALL-BUFFERS is non-nil, search also in non-file buffers."
  (interactive)
  (let* ((bufs (project-buffers (project-current t)))
         (bufs (if all-buffers
                   bufs
                 (seq-filter (lambda (buf)
                               (buffer-file-name buf))
                             bufs))))
    (multi-occur bufs regexp)))

(provide 'implicit-project)
