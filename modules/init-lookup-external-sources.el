
(defvar +lookup/external-sources-fn-alist '()
  "Alist of (SOURCE-NAME . LOOKUP-FUNCTION).")


(defmacro +lookup/def-src! (source arglist &rest body)
  ""
  `(progn (defun ,(intern (concat "+lookup--" source-name)) ,arglist
	    ,body)
	  (add-to-list +lookup/external-sources ,(intern (concat "+lookup--" source-name)))))


(defun +lookup--consult-handler (source-name)
  (funcall-interactively (alist-get source-name '+lookup/external-sources)))

(defun +lookup/external ()
  ""
  (consult--read (mapc 'car '+lookup/external-sources)
		 :prompt "Source: "
		 :require-match t
		 :lookup '+lookup--consult-handler))

(provide 'init-lookup-external-sources)
