;;; -*- lexical-binding: t -*-

(defvar +modules/path (concat user-emacs-directory "modules"))

(add-to-list 'load-path +modules/path)

(defvar +modules/loaded-list '()
  "List of features loaded from modules.")

(defmacro +require! (mod-symbol &rest opts)
  (let* ((should-load? (if (plist-member opts :if)
                           (plist-get opts :if)
                         t)))
    `(when ,should-load?
       (add-to-list '+modules/loaded-list ,mod-symbol)
       (require ,mod-symbol))))

(defmacro +module/declare! (name)
  "Set up `load-path' for a subdirectory in `+modules/path'."
  (let ((var (intern (concat "+module/" (symbol-name name) "-path")))
        (path (substitute-in-file-name (concat +modules/path "/" (symbol-name name)))))
    `(progn (defvar ,var ,(concat +modules/path "/" (symbol-name name)))
            (add-to-list 'load-path (symbol-value ',var)))))

(defun +modules/browse ()
  "Open user init module file."
  (interactive)
  (consult--read (mapcar (lambda (path)
                           (string-trim-left path (format "^%s/" +modules/path)))
                         (-filter (lambda (path)
                                    (string-suffix-p ".el" path))
                                  (directory-files-recursively +modules/path ".*\\.el")))
                 :prompt "Open init module file: "
                 :require-match t
                 :lookup (lambda (name &rest args)
                           (message (format "entry is %s" (file-name-concat +modules/path name)))
                           (find-file (file-name-concat +modules/path name)))))

(defun +modules/ripgrep ()
  (interactive)
  (consult-ripgrep +modules/path))

(defun +config/ripgrep ()
  (interactive)
  (consult-ripgrep user-emacs-directory))

;; (with-eval-after-load 'init-keybindings
;;   (+leader-keys
;;     "f P" '("Open module files" . +modules/browse)
;;     "f p *" '("Ripgrep in emacs dir" . +config/ripgrep)))

(provide 'init-modules)
