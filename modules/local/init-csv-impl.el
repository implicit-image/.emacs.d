

;;;###autoload
(defun +csv-mode--setup ()
  "setup `csv-mode' options for 2da files."
  (cond ((string-equal (file-name-extension (buffer-file-name)) "2da")
         (save-mark-and-excursion
           (when (null csv-align-mode)
             (csv-align-mode 1))
           (goto-char (point-min))
           ;; find row with id 0
           (search-forward-regexp "^0 ")
           ;; go line up to header line
           (previous-line)
           ;; set csv header
           (csv-header-line t)))
        (t nil)))


(provide 'init-csv-impl)
