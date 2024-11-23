(use-package calibredb
  :init
  (evil-set-initial-state 'calibredb-search-mode 'emacs)
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
  (calibredb-search-mode . (lambda ()
			     (interactive)
			     (display-line-numbers-mode -1)
			     (blink-cursor-mode -1)))
  :general
  (+leader-keys
    "a c" '("Open calibre" . calibredb))
  (calibredb-search-mode-map
   :states '(normal)
   "q" 'quit-window))

(use-package org-noter
  :init
  (setq org-noter-default-notes-file-names '("booknotes.org" "notes.org")
	org-noter-notes-search-path '("~/org/booknotes")
	org-noter-default-heading-title  "page $p$"
	org-noter-auto-save-last-location t
	org-noter-insert-selected-text-inside-note t)
  :general
  (pdf-view-mode-map
   :states '(normal)
   "g n" 'org-noter))

(use-package pdf-tools
  :init
  (evil-set-initial-state 'pdf-view-mode 'normal)
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-tools-install))
  :hook
  (pdf-view-mode . (lambda ()
		     (interactive)
		     (display-line-numbers-mode -1)))
  :general
  (pdf-view-mode-map
   :states '(normal)
   "] ]" 'pdf-view-next-page
   "[ [" 'pdf-view-previous-page
   "j" 'pdf-view-scroll-up-or-next-page
   "k" 'pdf-view-scroll-down-or-previous-page
   "=" 'pdf-view-enlarge
   "-" 'pdf-view-shrink
   "C-=" 'pdf-view-center-in-window
   "q" 'kill-this-buffer))


(use-package nov
  :init (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package djvu)

(provide 'init-books)
