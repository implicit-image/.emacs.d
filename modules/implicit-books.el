
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-tools-install))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))


(use-package calibredb
  :config
  (setq calibredb-root-dir "~/library"
        calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir)
        calibredb-library-alist '(("~/library"))
        calibredb-search-page-max-rows 50
        calibredb-virtual-library-alist '(("Economics" . "economics")
                                          ("Theory" . "theory")
                                          ("Maths" . "maths")
                                          ("Computer Science" . "cs"))
        calibredb-format-all-the-icons t
        calibredb-format-icons-in-terminal t
        calibredb-format-character-icons t)
  :hook
  (calibredb-search-mode . (lambda () (display-line-numbers-mode -1))))

(use-package org-noter
  :init
  (setq org-noter-default-notes-file-names '("booknotes.org" "notes.org")
	org-noter-notes-search-path '("~/org/booknotes")
	org-noter-default-heading-title  "page $p$"
	org-noter-auto-save-last-location t
	org-noter-insert-selected-text-inside-note t))

(use-package pdf-tools
  :init
  (evil-set-initial-state 'pdf-view-mode 'normal)
  :hook
  (pdf-view-mode . (lambda () (display-line-numbers-mode -1))))
 

(use-package nov)

(use-package djvu)

(provide 'implicit-books)
