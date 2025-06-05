

;;;###autoload
(defun +checkers/flymake-toggle-eol ()
  (interactive)
  (when (bound-and-true-p flymake-mode)
    (+toggle-local-var! flymake-show-diagnostics-at-end-of-line 'short nil)
    (flymake-mode 1)))

(provide 'init-checkers-impl)
