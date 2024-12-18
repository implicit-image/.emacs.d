;;; -*- lexical-binding: t -*-

(use-package org-contrib)
(use-package htmlize)
(use-package ox-rss)
(use-package org-roam-ui)

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-pretty-table
  :straight (org-pretty-table :type git
			       :host github
			       :repo "Fuco1/org-pretty-table")
  :hook
  (org-mode . org-pretty-table-mode))

(use-package el-easydraw
  :straight (el-easydraw :type git
			 :host github
			 :repo "misohena/el-easydraw")
  :functions
  (edraw-org-setup-default edraw-org-setup-exporter)
  :config
  (with-eval-after-load "ox"
    (require 'edraw-org)
    (edraw-org-setup-exporter))
  :hook
  (org-mode . edraw-org-setup-default))

(use-package org-xopp
  :straight (org-xopp :type git
		      :host github
		      :repo "mahmoodsh36/org-xopp"))

(use-package toc-org
  :hook
  (org-mode . toc-org-mode))

(use-package org-special-block-extras
  :hook (org-mode . org-special-block-extras-mode)
  :custom
    (o-docs-libraries
     '("~/org-special-block-extras/documentation.org")
     "The places where I keep my ‘#+documentation’"))

(use-package org
  :init
  (setq org-confirm-babel-evaluate nil
	org-startup-indented nil
	org-pretty-entities t
	org-use-sub-superscripts "{}"
	org-startup-with-inline-images t
	org-image-actual-width t
	+org/agenda-file (substitute-in-file-name "$HOME/org/agenda/agenda.org")
	+org/tasks-file (substitute-in-file-name "$HOME/org/agenda/tasks.org")
	+org/journal-file (substitute-in-file-name "$HOME/org/journal.org")
	+org/metrics-file (substitute-in-file-name "$HOME/org/metrics.org"))
  :config
  (plist-put org-format-latex-options :background "#181818")
  (plist-put org-format-latex-options :foreground "#f1f1f1")
  (plist-put org-format-latex-options :scale 2.0)
  (setq org-hide-emphasis-markers 1
        org-directory "~/org/"
        org-hide-macro-markers 1
        org-latex-packages-alist '(("" "color" t) ("" "tikz" t))
        org-latex-compiler "pdflatex"
        org-confirm-babel-evaluate nil
        org-md-headline-style 'setext
        org-odt-preferred-output-format "doc"
	org-return-follows-link t
	org-latex-compiler "lualatex"
        ;; agenda
        org-log-done 'time
        org-log-into-drawer t)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("archive.org" :maxlevel . 1)
          ("tasks.org" :maxlevel . 1)))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '((:startgroup)
					; Put mutually exclusive tags here
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
	  ("batch" . ?b)
          ("note" . ?n)
	  ("idea" . ?i)))

  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp ,+org/tasks-file "TODOS")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
	  ("tn" "Next Task" entry (file+headline +org/tasks-file "Tasks")
	   "** NEXT %? \nDEADLINE: %t")
          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree +org/journal-file)
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree +org/journal-file)
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree +org/journal-file)
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline +org/metrics-file "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))
  :hook
  (org-mode . visual-line-mode)
  :general
  (org-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "t l" '("Toggle latex preview" . org-latex-preview)
   "t p" '("Toggle pretty symbols" . org-toggle-pretty-entities)
   "m i t" '("Create table" . org-table-create-or-convert-from-region)
   "m l" '(:ignore t :which-key "link")
   "m l i" '("Insert link" . org-insert-link)))

(provide 'init-org)
