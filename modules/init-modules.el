(defun +modules/browse ()
  (interactive)
  (counsel-find-file "" +modules/path))

(+leader-keys
  "f P" '("Browse modules" . +modules/browse))

(provide 'init-modules)
