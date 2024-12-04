;;; -*- lexical-binding: t -*-

(use-package org
  :init
  (setq org-src-tab-acts-natively t
	org-edit-src-content-indentation 0
	org-src-preserve-indentation nil)
  :config
  (+windows-cfg
   '(("\*Org-Babel\**" "\*Org Src\**")
     :regexp t :height 0.25 :position bottom :dedicated nil))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (R . t)
     (python . t)
     (shell . t)
     (emacs-lisp . t)
     (rust . t)
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
  (add-to-list 'org-src-lang-modes (cons "nwscript" 'nwscript-mode)))

(use-package jupyter
  :hook
  (org-mode . (lambda ()
		(require 'ob-jupyter)
		(org-babel-jupyter-override-src-block "haskell"))))

(provide 'init-org-babel)
