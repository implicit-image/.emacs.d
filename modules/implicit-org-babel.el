(use-package org
  :init
  (setq org-src-tab-acts-natively t
	org-edit-src-content-indentation 0
	org-src-preserve-indentation nil)
  :config
  (push '("\*Org-Babel\**"
	  :regexp t
	  :height 0.25
	  :position bottom
	  :dedicated nil) popwin:special-display-config)
  ;; org src block special edit
  (push '("\*Org Src\**"
	  :regexp t
	  :height 0.25
	  :position bottom
	  :dedicated nil) popwin:special-display-config)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (R . t)
     (python . t)
     (shell . t)
     (emacs-lisp . t)
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
  :after org/org-src
  :config
  (org-babel-jupyter-override-src-block "haskell"))



(provide 'implicit-org-babel)
