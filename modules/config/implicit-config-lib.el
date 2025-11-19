;;; -*- lexical-binding: t -*-
(require 'cl-lib)

(defvar-local +indent-tab-function 'indent-for-tab-command)

(defvar-local +search-buffer-function 'isearch-forward)

(defvar-local +lookup-definition-function 'xref-find-definitions)

(defvar-local +lookup-references-function 'xref-find-references)

(defvar-local +lookup-documentation-function 'eldoc-doc-buffer)

(defvar-local +mode-transient-menu '+transient--default-menu)

(defvar +buffer-functions-var-alist '((+indent-tab-function . :indent-tab)
                                      (+search-buffer-function . :search-buffer)
                                      (+lookup-definition-function . :lookup-definition)
                                      (+lookup-references-function . :lookup-references)
                                      (+lookup-documentation-function . :lookup-documentation)
                                      (compile-command . :compile-command)
                                      (+mode-transient-menu . :transient-menu)))

(defvar +package-deps-alist nil)

(defmacro +mapcar-non-nil! (function sequence)
  "Map a FUNCTION over a SEQUENCE and return a list of results with only non-nil values."
  `(seq-filter (lambda (e)
                 e)
               (seq-map ,function
                        ,sequence)))


(defmacro +contrast-color! (color)
  "Return a color contrasting well with COLOR."
  `(apply
    ,(if (< (color-distance color "#000000") 180000)
         'doom-lighten
       'doom-darken)
    color
    0.3
    nil))

(defmacro +toggle-var! (var &optional val1 val2)
  "Toggle variable VAR between VAL1 and VAL2. If VAL1 is not supplied, its\
value is `t'. If VAL2 is not supplied, its value is `nil'."
  (let ((val1 (or val1
                  t))
        (val2 (or val2
                  nil)))
    `(setopt ,var (if (eq ,var ,val1) ,val2 ,val1))))


(defmacro +toggle-local-var! (var &optional val1 val2)
  "Toggle variable VAR between VAL1 and VAL2. If VAL1 is not supplied, its\
value is `t'. If VAL2 is not supplied, its value is `nil'."
  (let ((val1 (or val1
                  t))
        (val2 (or val2
                  nil)))
    `(setq-local ,var (if (eq ,var ,val1) ,val2 ,val1))))

(defmacro +lookup-set-fn! (type &rest forms)
  "Set lookup function of TYPE according to FORMS."
  (let ((fn-list (pcase type
                   ('popup '+lookup/popup-functions-alist)
                   ('buffer '+lookup/buffer-functions-alist)
                   ('ref '+lookup/find-ref-functions-alist)
                   ('def '+lookup/find-def-functions-alist)
                   ('impl '+lookup/find-impl-functions-alist)
                   (_ nil))))
    (when fn-list
      `(mapc (lambda (form)
               (add-to-list ',fn-list form))
             ',forms))))
;;;###autoload
(defmacro +os/per-system! (&rest args)
  (let ((val (cond ((+os/is-windows-p) (plist-get args :win))
                   ((+os/is-wsl-p) (plist-get args :wsl))
                   ((+os/is-linux-p) (plist-get args :linux)))))
    val))

(defmacro +autoloads! (file functions)
  "Setup autoloads for FUNCTIONS in FILE. Each function in FUNCTIONS can be either a\
symbol representing a function or a list where the first element is the function symbol\
and up to next 3 are values representing consecutive `autoload' arguments\
starting at the third one. The `autoload' docstring can be omitted."
  (let ((exprs (mapcar (lambda (fn)
                         (cond ((symbolp fn) `(autoload ',fn ,file))
                               ((listp fn)
                                (let ((func (car fn))
                                      (rest (cdr fn)))
                                  (cond ((and rest (not (stringp (car rest))))
                                         `(autoload ',func ,file ,@(take 2 rest)))
                                        (t `(autoload ',func ,file ,@(take 3 rest))))))))
                       functions)))
    `(progn ,@exprs)))

;;;###autoload
(defmacro +when-idle! (min-time &rest body)
  "Run BODY with next idle timer after MIN-TIME of idle time."
  (declare (indent defun))
  `(run-with-idle-timer ,min-time nil (lambda () ,@body)))

(defmacro +defconstruct! (name &rest body)
  "Define mode-line construct +mode-line-NAME with BODY."
  (declare (indent defun))
  (let ((name (intern (concat "+mode-line-" (symbol-name name)))))
    `(progn (defvar-local ,name
              ,@body)
            (put ',name 'risky-local-variable t))))

(defmacro +set-buffer-functions! (&rest args)
  "Set buffer local utility functions (see `+buffer-functions-var-alist')."
  (if  (not (eq (mod (length args) 2) 0))
      (error "Each option should have an associated value")
    `(progn ,@(+mapcar-non-nil!
               (lambda (cell)
                 (let* ((var (car cell))
                        (key (cdr cell))
                        (val (plist-get args key)))
                   (if val
                       `(setq-local ,var ,val)
                     val)))
               +buffer-functions-var-alist))))

(defmacro +add-directories-to-exec-path! (&rest dirs)
  `(progn ,@(mapcar (lambda (path)
                      `(add-to-list 'exec-path ,path))
                    dirs)))

(defun +from-shell--getenv-linux (varname)
  "Get environment variable VARNAME in SHELL. If shell is nil, use `'."
  (let* ((shell-file-name (or (executable-find "zsh")
                              (executable-find "bash"))) ;; TODO: add handling for nonstandard shells
         (var (string-trim (shell-command-to-string (format "echo $%s" varname)))))
    (if (string-empty-p var)
        (getenv varname)
      var)))

(defun +getenv-from-shell (varname)
  "Get environment variable VARNAME from shell."
  (if (or (+os/is-linux-p)
          (+os/is-wsl-p))
      (+from-shell--getenv-linux varname)
    (user-error "Wrong OS")))

;;;###autoload
(defun +set-exec-path-from-shell ()
  "Add entries from $PATH to variable `exec-path'."
  (let ((paths (+getenv-from-shell "PATH")))
    (if (string-empty-p paths)
        (user-error "$PATH empty")
      (mapc (lambda (path)
              (message path)
              (add-to-list 'exec-path path))
            (string-split paths ":")))))
;;;###autoload
(defun +set-env-vars-from-shell (&rest vars)
  "Set all environment variables in VARS."
  (let ((assocs (mapcar
                 (lambda (assoc)
                   (let* ((binding (string-split (string-trim assoc) "="))
                          (symbol (car binding))
                          (val (cadr binding)))
                     (cons symbol val)))
                 (string-split (shell-command-to-string "env") "\n"))))
    (dolist (assoc assocs)
      (when (memq (car assoc) vars)
        (setenv (car assoc) (cadr assoc))))))

(defun +char-whitespace? (char)
  "Check if CHAR is whitespace."
  (or (eq char 32)
      (eq char 160)
      (eq char 9)))

;;;###autoload
(defun +os/is-windows-p ()
  "Return t if current system is MS windows."
  (eq system-type 'windows-nt))

;;;###autoload
(defun +os/is-linux-p ()
  "Return t if current system is GNU/Linux."
  (eq system-type 'gnu/linux))

;;;###autoload
(defun +os/is-wsl-p ()
  "Return t if current system is probably WSL, nil otherwise."
  (and (+os/is-linux-p)
       (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop")))

(defun +lookup/definition ()
  (interactive)
  (command-execute +lookup-definition-function))

(defun +lookup/references ()
  (interactive)
  (command-execute +lookup-references-function))

(defun +lookup/implementation ()
  (interactive)
  (message "not implemented yet!"))

(defun +lookup/web ()
  (interactive)
  (message "not implemented yet!"))

(defun +lookup/documentation (&optional popup-window)
  (interactive "P")
  (command-execute +lookup-documentation-function))

(defmacro ii/packages! (&rest pkgs)
  "Expands to sequential `use-package' declarations with PKGS as arguments,."
  `(progn ,@(mapcar (lambda(pkg-sym)
                      (if (symbolp pkg-sym)
                          `(straight-use-package ',pkg-sym)
                        (when (listp pkgs-sym)
                          `(use-package ,@pkg-sym))))
                    pkgs)))

(defmacro ii/per-machine! (&rest args)
  ""
  (let ((system-sym (intern (concat ":" (system-name)))))
    `(progn ,@(plist-get args system-sym))))

(provide 'implicit-config-lib)
