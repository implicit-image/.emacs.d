

;;;###autoload
(defun +search/rg-thing-at-point ()
  (interactive)
  (let ((s (symbol-at-point)))
    (consult-ripgrep
     (cond ((bound-and-true-p projectile-project-root) (projectile-project-root))
           ((bound-and-true-p project-root) (project-root))
           (t default-directory))
     (if (eq s nil) "" (symbol-name s)))))

;;;###autoload
(defun +next-error-no-select (&optional n)
  (interactive "p")
  (save-selected-window
    (let ((next-error-highlight next-error-highlight-no-select)
          (display-buffer-overriding-action
           '((display-buffer-use-least-recent-window)
             (inhibit-same-window . t))))
      (next-error n))))

;;;###autoload
(defun +previous-error-no-select (&optional n)
  (interactive "p")
  (+next-error-no-select (- (or n 1))))

;;;###autoload
(defun +deadgrep-display-buffer-function (buffer)
  (display-buffer-same-window buffer nil))

(provide 'init-search-impl)
