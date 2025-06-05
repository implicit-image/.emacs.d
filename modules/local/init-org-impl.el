(require 'ob)
(require 'consult)
(require 'ob-jupyter)

;;;###autoload
(defun +org-roam-mode--setup ()
  (setq-local org-attach-id-dir (expand-file-name "data" org-roam-directory)))

;;;###autoload
(defun +org-babel--setup ()
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
     (sqlite . t))))

;;;###autoload
(defun +org/rg-in-roam-notes ()
  (interactive)
  (consult-ripgrep org-roam-directory ""))

;;;###autoload
(defun +org-template-fn--file-in-subdir (&optional dir)
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

;;;###autoload
(defun +org-mode--jupyter-setup ()
  (org-babel-jupyter-override-src-block "haskell"))


(provide 'init-org-impl)
