
(defvar +lookup/external-sources-fn-alist '()
  "Alist of (SOURCE-NAME . LOOKUP-FUNCTION).")

(defvar +lookup/external-sources nil)

(defmacro +lookup/def-src! (source arglist doc &rest body)
  "Declare external information source SOURCE to lookup query in."
  (declare (indent defun)
           (doc-string 3))
  (let ((name (intern (concat "+lookup--" source))))
    `(progn (defun ,name (,@arglist)
              (interactive)
              ,@body)
            (add-to-list '+lookup/external-sources '(,source . ,name)))))

(+lookup/def-src! "wikipedia" (a b)
  "Lookup query on wikipedia."
  (message "dsfsdfsdf"))

(defun +lookup/external (&optional at-point)
  "Lookup query in one of `+lookup/external-sources'."
  (interactive "P")
  (let* ((source (completing-read "Source: "
                                  (mapc 'car +lookup/external-sources)))
         (query (if (bound-and-true-p at-point)
                    (thing-at-point 'sexp t)
                  (read-string "Query: ")))
         (handler (alist-get source +lookup/external-sources)))
    (funcall-interactively handler)))

(+leader-keys
  "s X" '("Lookup in external source" . +lookup/external))

(provide 'init-lookup-external-sources)
