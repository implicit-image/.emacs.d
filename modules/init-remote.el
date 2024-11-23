;;;###autoload
(defun +remote/counsel-find-file ()
  (interactive)
  (ivy-read "Remote method:" (mapcar (lambda (method-form) (car method-form)) tramp-methods)
	    :preselect 0
	    :require-match t
	    :history '+remote/counsel-find-file-history
	    :action (lambda (method)
		      (interactive)
		      (counsel-find-file (string-join `("/" ,method "::"))))
	    :caller '+remote/counsel-find-file))

;;;###autoload
(defun +remote/open-file-as-sudo ()
  (interactive)
  (counsel-find-file "/sudo::"))

(use-package auth-source
  :demand
  :config
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")))

(use-package tramp
  :general
  (+leader-keys
    "f s" '("Open as sudo" . +remote/open-file-as-sudo)
    "f S" '("Open remote file" . +remote/counsel-find-file)))


(provide 'init-remote)
