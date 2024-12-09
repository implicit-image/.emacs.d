;;; -*- lexical-binding: t -*-

(use-package calibredb
  :init
  (evil-set-initial-state 'calibredb-search-mode 'normal)
  (evil-set-initial-state 'calibredb-show-mode 'normal)
  ;; (+windows-cfg '((calibredb-show-mode)
  ;; 		  :position right :width 0.5 :dedicated t :noselect nil :stick nil))
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
  (calibredb-show-mode . (lambda ()
			   (interactive)
			   (display-line-numbers-mode -1)))
  :general
  (+leader-keys
    "a c" '("Open calibre" . calibredb))
  (calibredb-search-mode-map
   :states '(normal visual)
   "TAB" 'calibredb-toggle-view-at-point
   "RET" 'calibredb-find-file
   "*" 'calibredb-toggle-favorite-at-point
   "," 'calibredb-quick-look
   "." 'calibredb-dired-open
   "/" 'calibredb-search-live-filter
   "?" 'calibredb-dispatch
   "A" 'calibredb-add-dir
   "D" 'calibredb-remove-marked-items
   "L" 'calibredb-library-list
   "N" 'calibredb-library-next
   "O" 'calibredb-find-file-other-frame
   "P" 'calibredb-library-previous
   "R" 'calibredb-search-clear-filter
   "S" 'calibredb-switch-library
   "V" 'calibredb-open-file-with-default-tool
   "a" 'calibredb-add
   "b" 'calibredb-catalog-bib-dispatch
   "c" 'calibredb-clone
   "d" 'calibredb-remove
   "e" 'calibredb-export-dispatch
   "g" 'calibredb-filter-dispatch
   "h" 'calibredb-toggle-highlight-at-point
   "i" 'calibredb-edit-annotation
   "j" 'calibredb-next-entry
   "k" 'calibredb-previous-entry
   "l" 'calibredb-virtual-library-list
   "m" 'calibredb-mark-and-forward
   "n" 'calibredb-virtual-library-next
   "o" 'calibredb-sort-dispatch
   "p" 'calibredb-virtual-library-previous
   "q" 'calibredb-search-quit
   "r" 'calibredb-search-refresh-and-clear-filter
   "s" 'calibredb-set-metadata-dispatch
   "u" 'calibredb-unmark-and-forward
   "v" 'calibredb-view
   "x" 'calibredb-toggle-archive-at-point
   "y" 'calibredb-yank-dispatch
   "DEL" 'calibredb-unmark-and-backward
   "<backtab>" 'calibredb-toggle-view
   "<mouse-3>" 'calibredb-search-mouse
   "q" 'quit-window)
  (calibredb-show-mode-map
   :states '(normal visual)
   "," 'calibredb-quick-look
   "." 'calibredb-open-dired
   "?" 'calibredb-entry-dispatch
   "O" 'calibredb-find-file-other-frame
   "V" 'calibredb-open-file-with-default-tool
   "e" 'calibredb-export-dispatch
   "o" 'calibredb-find-file
   "q" 'calibredb-entry-quit
   "ESC" 'calibredb-entry-quit
   "s" 'calibredb-set-metadata-dispatch
   "y" 'calibredb-yank-dispatch
   "C-c s A" 'calibredb-set-metadata--authors
   "c-c s T" 'calibredb-set-metadata--title
   "C-c s a" 'calibredb-set-metadata--author_sort
   "C-c s c" 'calibredb-set-metadata--comments
   "C-c s t" 'calibredb-set-metadata--tags))

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
