;;; -*- lexical-binding: t -*-


(use-package calibredb
  :custom
  (calibredb-root-dir "~/library")
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
  :hook
  (calibredb-search-mode-hook . (lambda ()
                                  (interactive)
                                  (blink-cursor-mode -1))))

;; :general
;; (+leader-keys
;;   "a C" '("Open calibre book" . calibredb-consult-read)
;;   "a c" '("Open calibre" . calibredb))
;; (calibredb-search-mode-map
;;  :states '(normal visual)
;;  "TAB" 'calibredb-toggle-view-at-point
;;  "RET" 'calibredb-find-file
;;  "*" 'calibredb-toggle-favorite-at-point
;;  "," 'calibredb-quick-look
;;  "." 'calibredb-dired-open
;;  "/" 'calibredb-search-live-filter
;;  "?" 'calibredb-dispatch
;;  "A" 'calibredb-add-dir
;;  "D" 'calibredb-remove-marked-items
;;  "L" 'calibredb-library-list
;;  "N" 'calibredb-library-next
;;  "O" 'calibredb-find-file-other-frame
;;  "P" 'calibredb-library-previous
;;  "R" 'calibredb-search-clear-filter
;;  "S" 'calibredb-switch-library
;;  "V" 'calibredb-open-file-with-default-tool
;;  "a" 'calibredb-add
;;  "b" 'calibredb-catalog-bib-dispatch
;;  "c" 'calibredb-clone
;;  "d" 'calibredb-remove
;;  "e" 'calibredb-export-dispatch
;;  "g" 'calibredb-filter-dispatch
;;  "h" 'calibredb-toggle-highlight-at-point
;;  "i" 'calibredb-edit-annotation
;;  "j" 'calibredb-next-entry
;;  "k" 'calibredb-previous-entry
;;  "l" 'calibredb-virtual-library-list
;;  "m" 'calibredb-mark-and-forward
;;  "n" 'calibredb-virtual-library-next
;;  "o" 'calibredb-sort-dispatch
;;  "p" 'calibredb-virtual-library-previous
;;  "q" 'calibredb-search-quit
;;  "r" 'calibredb-search-refresh-and-clear-filter
;;  "s" 'calibredb-set-metadata-dispatch
;;  "u" 'calibredb-unmark-and-forward
;;  "v" 'calibredb-view
;;  "x" 'calibredb-toggle-archive-at-point
;;  "y" 'calibredb-yank-dispatch
;;  "DEL" 'calibredb-unmark-and-backward
;;  "<backtab>" 'calibredb-toggle-view
;;  "<mouse-3>" 'calibredb-search-mouse
;;  "q" 'quit-window)
;; (calibredb-show-mode-map
;;  :states '(normal visual)
;;  "," 'calibredb-quick-look
;;  "." 'calibredb-open-dired
;;  "?" 'calibredb-entry-dispatch
;;  "O" 'calibredb-find-file-other-frame
;;  "V" 'calibredb-open-file-with-default-tool
;;  "e" 'calibredb-export-dispatch
;;  "o" 'calibredb-find-file
;;  "q" 'calibredb-entry-quit
;;  "ESC" 'calibredb-entry-quit
;;  "s" 'calibredb-set-metadata-dispatch
;;  "y" 'calibredb-yank-dispatch
;;  "C-c s A" 'calibredb-set-metadata--authors
;;  "c-c s T" 'calibredb-set-metadata--title
;;  "C-c s a" 'calibredb-set-metadata--author_sort
;;  "C-c s c" 'calibredb-set-metadata--comments
;;  "C-c s t" 'calibredb-set-metadata--tags))

(use-package org-noter
  :custom
  (org-noter-default-notes-file-names '("booknotes.org" "notes.org"))
  (org-noter-notes-search-path '("~/org/booknotes"))
  (org-noter-default-heading-title  "page $p$")
  (org-noter-auto-save-last-location t)
  (org-noter-kill-frame-at-session-end nil)
  (org-noter-always-create-frame nil)
  (org-noter-insert-selected-text-inside-note t)
  :init
  (defun +books/org-noter-init-session (&optional local-elem)
    "Select a document from calibre and start org-noter sesion with it."
    (interactive)
    (if (not (eq major-mode 'org-mode))
        (message "Org noter has to be activated in Org mode buffer.")
      (require 'org-noter)
      (require 'calibredb)
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
      (org-noter))))


;; :bind
;; (
;;  ;; ( :map 'pdf-view-mode-map
;;  ;;   ("g n" . 'org-noter)
;;  :map 'org-noter-doc-mode-map
;;  ;; ("i" . 'org-noter-insert-note)
;;  ;; ("i" . 'org-noter-insert-precise-note)
;;  ("C-c q" . 'org-noter-kill-session)
;;  :map 'org-noter-notes-mode-map
;;  ("C-c n q" . 'org-noter-kill-session)
;;  ("C-c n n" . '+books/org-noter-init-session)))

;; :general
;; (pdf-view-mode-map
;;  :states '(normal)
;;  "g n" 'org-noter)
;; (org-noter-doc-mode-map
;;  :states '(normal visual)
;;  "i" 'org-noter-insert-note
;;  "I" 'org-noter-insert-precise-note)
;; (org-noter-doc-map
;;  :states '(normal visual)
;;  :prefix "SPC m"
;;  :global-prefix "M-SPC m"
;;  "q" 'org-noter-kill-session
;;  "q" 'org-noter-kill-session)
;; (org-noter-notes-mode-map
;;  :states '(normal visual)
;;  "C-c n q" 'org-noter-kill-session
;;  "SPC m q" 'org-noter-kill-session)
;; (org-mode-map
;;  :states '(normal visual)
;;  :prefix "SPC m"
;;  :global-prefix "M-SPC m"
;;  "n" '("Start Org Noter session" . +books/org-noter-init-session)))

(use-package pdf-tools
  :custom
  (pdf-outline-imenu-use-flat-menus t)
  :mode ("\\.pdf\\'" . pdf-tools-install))
;; :bind
;; ( :map 'pdf-view-mode-map
;;   ("] ]" . pdf-view-next-page)
;;   ("[ [ " . pdf-view-prev-page)
;;   ("q" . kill-current-buffer)))
;; :general
;; (pdf-view-mode-map
;;  :states '(normal)
;;  "] ]" 'pdf-view-next-page
;;  "[ [" 'pdf-view-previous-page
;;  "j" 'pdf-view-next-line-or-next-page
;;  "k" 'pdf-view-previous-line-or-previous-page
;;  "=" 'pdf-view-enlarge
;;  "-" 'pdf-view-shrink
;;  "C-=" 'pdf-view-center-in-window
;;  "q" 'kill-this-buffer))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :hook
  (nov-mode-hook . variable-pitch-mode))

(use-package djvu)

(provide 'init-books)
