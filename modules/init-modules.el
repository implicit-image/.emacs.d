;;; -*- lexical-binding: t -*-


(defvar +modules/path "")

(setq +modules/path (concat user-emacs-directory "modules"))

(add-to-list 'load-path +modules/path)

;; setup submodule load paths
(setq +modules/ide-path (concat +modules/path "/ide"))
(add-to-list 'load-path +modules/ide-path)

(setq +modules/org-path (concat +modules/path "/org"))
(add-to-list 'load-path +modules/org-path)

(setq +modules/lang-path (concat +modules/path "/lang"))
(add-to-list 'load-path +modules/lang-path)

(setq +modules/llm-path (concat +modules/path "/llm"))
(add-to-list 'load-path +modules/llm-path)

(defvar +modules/loaded-list '()
  "List of features loaded from modules.")

(defmacro +modules/require! (mod-symbol &rest opts)
  "Define a module deriving feature NAME and load it with OPTS."
  `(progn (let* ((mod-file-name (concat +modules/path (symbol-name ,mod-symbol) ".el"))
                 (add-to-list '+modules/loaded-list
                              (mod-symbol . mod-file-name) )
                 (require ,mod-symbol)))))

(defmacro +modules/module-loaded-p! (module)
  "Return t if the module MODULE is loaded and nil otherwise."
  `(memq )
  `(not (not (alist-get ,module +modules/provided-module-features))))

(defmacro +module/declare! (name)
  `(progn (defvar ,(concat "+modules/" (symbol-name name) "-path"))))

(defun +modules/browse ()
  "Open user init module file."
  (interactive)
  (require 'f)
  (consult--read (mapcar (lambda (path)
                           (string-trim-left path (format "^%s/" +modules/path)))
                         (-filter (lambda (path)
                                    (string-suffix-p ".el" path))
                                  (directory-files-recursively +modules/path ".*\\.el")))
                 :prompt "Open init module file: "
                 :require-match t
                 :lookup (lambda (name &rest args)
                           (message (format "entry is %s" (f-join +modules/path name)))
                           (find-file (f-join +modules/path name)))))

(defun +modules/ripgrep ()
  (interactive)
  (consult-ripgrep +modules/path))


(with-eval-after-load 'init-keybindings
  (+leader-keys
    "f P" '("Open module files" . +modules/browse)
    "f p *" '("Ripgrep in module dir" . +modules/ripgrep)))

(provide 'init-modules)
