

(use-package csv-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.2da\\'" . csv-mode))

  (defun +csv/setup ()
    "setup `csv-mode' options for 2da files."
    (interactive)
    (cond ((string-equal (file-name-extension (buffer-file-name)) "2da")
           (csv-align-mode 1)
           ;; find row with id 0
           (search-forward-regexp "^0 ")
           ;; go line up to header line
           (previous-line)
           ;; set csv header
           (csv-header-line t))))

  :hook
  (csv-mode . +csv/setup))

(provide 'init-csv)
