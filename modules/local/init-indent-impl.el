;;;###autoload
(defun +indent-bars-mode--setup ()
  (when (not (memq major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     racket-mode
                     scheme-mode
                     common-lisp-mode)))
    (indent-bars-mode)))

(provide 'init-indent-impl)
