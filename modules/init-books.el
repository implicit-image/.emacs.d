;;; -*- lexical-binding: t -*-

(use-package calibredb
  :custom
  (calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (calibredb-library-alist '(("~/library")))
  (calibredb-search-page-max-rows 50)
  (calibredb-virtual-library-alist '(("Economics" . "economics")
                                     ("Theory" . "theory")
                                     ("Maths" . "maths")
                                     ("Computer Science" . "cs")))
  (calibredb-format-all-the-icons t)
  (calibredb-format-icons-in-terminal t)
  (calibredb-format-character-icons t)
  :init
  (setq calibredb-root-dir (expand-file-name "~/library"))
  :functions
  (calibredb-candidates)
  :bind*
  ( :map meow-special-global-map
    ("c" . calibredb)
    ("C" . calibredb-consult-read)))

(use-package org-noter
  :custom
  (org-noter-default-notes-file-names '("booknotes.org" "notes.org"))
  (org-noter-notes-search-path '("~/org/booknotes"))
  (org-noter-default-heading-title  "page $p$")
  (org-noter-auto-save-last-location t)
  (org-noter-kill-frame-at-session-end nil)
  (org-noter-always-create-frame nil)
  (org-noter-insert-selected-text-inside-note t)
  :bind*
  ( :map pdf-view-mode-map
    ("g n" . org-noter)
    :map org-noter-doc-mode-map
    ("i" . org-noter-insert-note)
    ("i" . org-noter-insert-precise-note)
    ("C-c n q" . org-noter-kill-session)
    :map org-noter-notes-mode-map
    ("C-c n q" . org-noter-kill-session)
    ("C-c n n" . +books/org-noter-init-session)))


(use-package pdf-tools
  :custom
  (pdf-outline-imenu-use-flat-menus t)
  :init
  (setopt org-format-latex-header "\\documentclass{article}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\usepackage{xcolor}")
  :mode ("\\.pdf\\'" . pdf-tools-install))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook
  (nov-mode-hook . variable-pitch-mode))

(use-package djvu)

(provide 'init-books)
