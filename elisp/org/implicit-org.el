;; -*- lexical-binding: t; -*-
(require 'ob)
(require 'consult)
(require 'ob-jupyter)

(defvar +org-template-dir (expand-file-name "+templates" user-emacs-directory))

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

(defun +org/rg-in-roam-notes ()
  (interactive)
  (require 'org-roam)
  (consult-ripgrep org-roam-directory ""))

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

(defun ii/org-setup-src-mode ()
  (setq-local buffer-file-name nil
              default-directory ))

(defun ii/org-collect-blocks-with-header-arg (arg val)
  (let ((res nil))
    (org-babel-map-src-blocks nil
      (let* ((info (org-babel-get-src-block-info)))
        (when (eq val (cdr (assq arg (nth 2 info))))
          (push))))
    (nreverse res)))

(defun ii/org-edit-src-blocks-with-header-arg (arg val)
  (let ((blocks (ii/org-collect-blocks-with-header-arg arg val)))))

(defun ii/org-edit-src-block ())

;;;###autoload
(defun ii/org-narrow-to-heading ()
  (interactive))

(defun ii/org-narrow-to-next-heading (n)
  (interactive "p"))

;;;###autoload
(defun +org-mode--jupyter-setup ())

(defun ii/org-quick-setup ()
  (interactive)
  (setq-local default-directory "/home/b/programming/moder/"
              buffer-file-name "/home/b/programming/moder/test-script.py"))

(provide 'implicit-org)
