;;;###autoload
(defun +dired-mode--setup ()
  (toggle-truncate-lines 1)
  (setq-local case-fold-search nil))


(provide 'init-dired-impl)
