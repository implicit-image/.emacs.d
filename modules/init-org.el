;;; -*- lexical-binding: t -*-

(use-package org
  :straight `(org
              :fork (:host nil
                           :repo "https://git.tecosaur.net/tec/org-mode.git"
                           :branch "dev"
                           :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
                (require 'lisp-mnt)
                (let ((version
                       (with-temp-buffer
                         (insert-file-contents "lisp/org.el")
                         (lm-header "version")))
                      (git-version
                       (string-trim
                        (with-temp-buffer
                          (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                          (buffer-string)))))
                  (insert
                   (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                   (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                   "(provide 'org-version)\n")))
              :pin nil)
  :init
  (setq org-confirm-babel-evaluate nil
        org-startup-indented nil
        org-pretty-entities t
        org-highlight-latex-and-related '(latex native)
        org-use-sub-superscripts "{}"
        org-inhibit-logging t
        org-startup-with-inline-images t
        org-image-actual-width t
        org-latex-preview-live-throttle 0.5
        org-latex-preview-live-debounce 0.5
        org-latex-preview-live t
        org-latex-preview-appearance-options '(:foreground "#f1f1f1" :background "#181818" :scale 2.0 :zoom. 1.0 :page-width 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
        +org/agenda-file (substitute-in-file-name "$HOME/org/agenda/agenda.org")
        +org/tasks-file (substitute-in-file-name "$HOME/org/agenda/tasks.org")
        +org/journal-file (substitute-in-file-name "$HOME/org/journal.org")
        +org/metrics-file (substitute-in-file-name "$HOME/org/metrics.org"))

  :config
  (setq org-hide-emphasis-markers 1
        org-directory "~/org/"
        org-hide-macro-markers 1
        org-latex-packages-alist '(("" "color" t) ("" "tikz" t))
        org-confirm-babel-evaluate nil
        org-md-headline-style 'setext
        org-odt-preferred-output-format "doc"
        org-return-follows-link t
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

;;;; tags
  (setq org-tag-alist
        '((:startgroup)
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

;;;; capture templates
  (setq org-capture-templates
;;;; tasks
        `(("t" "Tasks / Projects")
;;;; add a task
          ("tt" "Task" entry (file+olp ,+org/tasks-file "TODOS")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
;;;; add next immidiate task
          ("tn" "Next Task" entry (file+headline +org/tasks-file "Tasks")
           "** NEXT %? \nDEADLINE: %t")
;;;; journal
          ("j" "Journal Entries")
;;;;
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
  (org-mode-hook . visual-line-mode)
  (org-mode-hook . (lambda ()
                     (interactive)
                     (whitespace-mode -1)))
  (org-mode-hook . org-latex-preview-auto-mode))
;; :general
;; (org-mode-map
;;  :states '(normal visual)
;;  :prefix "SPC"
;;  :global-prefix "M-SPC"
;;  "s i" '("Navigate Org headings" . consult-org-heading)
;;  "t l" '("Toggle latex preview" . org-latex-preview)
;;  "t p" '("Toggle pretty symbols" . org-toggle-pretty-entities)
;;  "m i t" '("Create table" . org-table-create-or-convert-from-region)
;;  "m l" '(:ignore t :which-key "link")
;;  "m l i" '("Insert link" . org-insert-link)))


(use-package org-contrib)

(use-package htmlize)

(use-package ox-rss)

(use-package org-roam-ui)

(use-package org-appear
  :hook (org-mode-hook . org-appear-mode))

(use-package org-pretty-table
  :straight (org-pretty-table :type git
                              :host github
                              :repo "Fuco1/org-pretty-table")
  :hook
  (org-mode-hook . org-pretty-table-mode))

(use-package org-xopp
  :straight (org-xopp :type git
                      :host github
                      :repo "mahmoodsh36/org-xopp"))

(use-package toc-org
  :hook
  (org-mode-hook . toc-org-mode))

(use-package org-special-block-extras
  :hook
  (org-mode-hook . org-special-block-extras-mode)
  :custom
  (o-docs-libraries
   '("~/org-special-block-extras/documentation.org")
   "The places where I keep my ‘#+documentation’"))

(defun +org-roam/rg-in-notes ()
  (interactive)
  (consult-ripgrep org-roam-directory ""))


(use-package org-roam
  :custom
  (org-roam-directory (substitute-in-file-name "$HOME/org/roam"))
  (org-roam-dailies-directory "daily")
  (org-roam-dailies-capture-templates '(("d" "default" entry
                                         "* %?"
                                         :target (file+head "%<%Y-%m-%d>.org"
                                                            "#+title: %Y=%m-%d\n"))))
  :init
  (defun +roam/template-fn--file-in-subdir (&optional dir)
    (let* ((root-dir (file-name-as-directory (file-name-concat (expand-file-name org-roam-directory)
                                                               (if (boundp 'dir)
                                                                   (concat dir "/")
                                                                 ""))))
           (subdir (read-directory-name "subdirectory: "
                                        root-dir))
           (filename (read-file-name "file: "
                                     (file-name-as-directory subdir))))
      (file-name-concat root-dir
                        subdir
                        filename)))

  (defun +roam/mode-setup ()
    "Setup `org-roam' related variables and modes."
    (interactive)
    (setq-local org-attach-id-dir (f-join org-roam-directory "data/")))
  :config
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)
  (require 'org-roam-export)


  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag))
        org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags))))
        org-roam-db-update-on-save t
        org-roam-completion-everywhere t
        org-roam-capture-templates
        `(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("b" "book notes" plain "%?"
           :if-new (file+head "booknotes/%^{filename}.org" "#+title: %^{title}\n")
           :unnarrowed t
           :jump-to-captured t)
          ("l" "learning" plain "%?"
           :target (file+head "%(+roam/template-fn--file-in-subdir \"/learning\")"
                              "#+title: %^{title}\n")
           :unnarrowed t)))
  :hook
  (org-roam-db-autosync-mode-hook . +roam/mode-setup))
;; :bind*
;; (("C-x <space> n r f" . org-roam-node-find)
;;  ("C-x <space> n r r" . org-roamm-node-random)
;;  ("C-x <space> n r s" . org-roam-db-sync)
;;  ("C-x <space> n r c" . org-roam-capture)
;;  ("C-x <space> n r *" . +org-roam/rg-in-notes)
;;  :map org-roam-mode-map
;;  ("C-c n i" . org-roam-node-insert)))

(use-package ob
  :straight nil
  :init
  (setq org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-src-preserve-indentation nil)

  (defvar +org-babel-temp-dir (file-name-concat (expand-file-name user-emacs-directory) "+org-babel"))

  (defun +ob--setup ()
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C . t)
       (R . t)
       (python . t)
       (shell . t)
       (emacs-lisp . t)
       ;; (rust . t)
       (awk . t)
       (calc . t)
       (clojure . t)
       (css . t)
       (ditaa . t)
       (forth . t)
       (gnuplot . t)
       (dot . t)
       (haskell . t)
       (java . t)
       (latex . t)
       (lisp . t)
       (lua . t)
       (makefile . t)
       (matlab . t)
       (js . t)
       (ocaml . t)
       (org . t)
       (perl . t)
       (scheme . t)
       (sql . t)
       (sqlite . t)))
    (add-to-list 'org-src-lang-modes (cons "jsx" 'rjsx))
    (add-to-list 'org-src-lang-modes (cons "nwscript" 'nwscript)))
  :hook
  (after-init-hook . +ob--setup))

(use-package jupyter
  :hook
  (org-mode-hook . (lambda ()
                     (require 'ob-jupyter)
                     (org-babel-jupyter-override-src-block "haskell"))))

(use-package ob-sql-mode)

(use-package org
  :init
  (setq org-agenda-files '("~/org/agenda/agenda.org"
                           "~/org/agenda/birthdays.org"
                           "~/org/agenda/habits.org"
                           "~/org/agenda/tasks.org")
        org-agenda-skip-unavailable-files t
        org-agenda-breadcrumbs-separator ">"
        org-agenda-start-with-log-mode t
        org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files))))))))


(provide 'init-org)
