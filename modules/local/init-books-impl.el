(require 'org-noter)
(require 'f)

;;;###autoload
(defun +books/org-noter-init-session (&optional local-elem)
  "Select a document from calibre and start org-noter sesion with it."
  (interactive)
  (if (not (eq major-mode 'org-mode))
      (message "Org noter has to be activated in Org mode buffer.")
    (if (bound-and-true-p local-elem)
        (org-up-element)
      (goto-line 1))
    (when (not (org-entry-get nil org-noter-property-doc-file))
      (consult--read (mapcar
                      (lambda (cand)
                        (let ((prop-list (cadr cand)))
                          `(,(format (s-join " " `("%s"
                                                   ,(propertize ":file"
                                                                'face 'font-lock-builtin-face)
                                                   "%s"))
                                     (or (car (alist-get :book-name prop-list))
                                         "")
                                     (or (car (alist-get :file-path prop-list))
                                         "")))))
                      (calibredb-candidates))
                     :prompt "File to annotate: "
                     :lookup (lambda (cand &rest args)
                               (let* ((path (string-trim-left cand ".*\:file "))
                                      (extensions (s-split "," (f-ext path)))
                                      (path-sans-ext ())
                                      (file (if (length= extensions 1)
                                                path
                                              (completing-read "Choose file:" (mapcar (lambda (ext) (concat)))))))
                                 (org-set-property org-noter-property-doc-file file)))))
    (save-buffer)
    (org-noter)))

(provide 'init-books-impl)
