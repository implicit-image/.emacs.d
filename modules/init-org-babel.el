;;; -*- lexical-binding: t -*-

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
  :config
  :hook
  (after-init-hook . +ob--setup))

(use-package jupyter
  :hook
  (org-mode-hook . (lambda ()
                     (require 'ob-jupyter)
                     (org-babel-jupyter-override-src-block "haskell"))))

(use-package ob-sql-mode)

(provide 'init-org-babel)
