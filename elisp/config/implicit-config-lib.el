;;; -*- lexical-binding: t -*-
(require 'cl-lib)

(defvar-local +search-buffer-function 'isearch-forward)

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
(defmacro ii/when-idle! (min-time &rest body)
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

;; taken from https://github.com/doomemacs/doomemacs/blob/38d94da67dc84897a4318714dcc48494c016d8c4/lisp/doom-lib.el
(defmacro ii/cmd! (&rest body)
  "Returns (lambda () (interactive) ,@body)
A factory for quickly producing interaction commands, particularly for keybinds
or aliases."
  (declare (doc-string 1))
  `(lambda (&rest _) (interactive) ,@body))

(defmacro ii/cmd!! (command &optional prefix-arg &rest args)
  "Returns a closure that interactively calls COMMAND with ARGS and PREFIX-ARG.
Like `cmd!', but allows you to change `current-prefix-arg' or pass arguments to
COMMAND. This macro is meant to be used as a target for keybinds (e.g. with
`define-key' or `map!')."
  (declare (doc-string 1) (pure t) (side-effect-free t))
  `(lambda (arg &rest _) (interactive "P")
     (let ((current-prefix-arg (or ,prefix-arg arg)))
       (,(if args
             #'funcall-interactively
           #'call-interactively)
        ,command ,@args))))

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
  (let ((proc (start-process "env-vars"
                             (get-buffer-create "* env-vars*")
                             shell-file-name
                             "-c"
                             "env")))
    (set-process-sentinel proc
                          (lambda (proc status)
                            (with-current-buffer (process-buffer proc)
                              (let ((assocs (mapcar
                                             (lambda (assoc)
                                               (let* ((binding (string-split (string-trim assoc) "="))
                                                      (symbol (car binding))
                                                      (val (cadr binding)))
                                                 (cons symbol val)))
                                             (string-split (buffer-substring-no-properties (point-min) (point-max)) "\n")))
                                    (inhibit-message t))
                                (dolist (assoc assocs)
                                  (let ((var (car assoc)))
                                    (if (string-equal var "PATH")
                                        (mapc (lambda (path)
                                                (when init-file-debug
                                                  (message path))
                                                (add-to-list 'exec-path path))
                                              (string-split (cdr assoc) ":"))
                                      (when (memq var vars)
                                        (setenv var (cadr assoc))))))))))))

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

(defun +lookup/documentation (&optional popup-window)
  (interactive "P")
  (command-execute +lookup-documentation-function))

(defmacro ii/packages! (&rest pkgs)
  "Expands to sequential `use-package' declarations with PKGS as arguments,."
  `(progn ,@(mapcar (lambda (pkg-sym)
                      (if (symbolp pkg-sym)
                          `(straight-use-package ',pkg-sym)
                        (when (listp pkg-sym)
                          `(use-package ,@pkg-sym))))
                    pkgs)))

(defmacro ii/per-machine! (&rest args)
  ""
  (let ((system-sym (intern (concat ":" (system-name)))))
    `(progn ,@(plist-get args system-sym))))

(defun ii/deps (&rest args)
  (eval
   `(and ,@(mapcar (lambda (exe)
                     `(let ((path (executable-find ,exe))
                            (inhibit-message t))
                        (message "DEPS: %s%s found%s"
                                 ,exe
                                 (if path "" " not")
                                 (if path (concat " " "at " path) "!"))
                        path))
                   args))))

(defmacro ii/with-no-message! (&rest body)
  `(let ((inhibit-message t))
     ,@body))

(defmacro ii/with-default-directory! (dir &rest body)
  (declare (indent defun))
  `(let ((default-directory ,dir))
     ,@body))

(defmacro ii/with-project-root! (&rest body)
  (declare (indent defun))
  `(let ((default-directory (or (project-root (project-current t))
                                default-directory)))
     ,@body))

(defmacro ii/eval-on-first-execution (func name how pred &rest body)
  (declare (indent defun))
  (let ((adv-symbol (intern (concat "ii/" name "--on-first-" (symbol-name func) "-" (symbol-name how) "-execution-setup"))))
    `(advice-add ',func ,how (defun ,adv-symbol (&rest args)
                               (when ,pred
                                 (advice-remove ',func ',adv-symbol)
                                 (message "Setting up %s" ,name)
                                 ,@body)))))

(defmacro ii/eval-on-first-hook (hook name pred &rest body)
  (declare (indent defun))
  (let ((func-symbol (intern (concat "ii/" name "--on-first-" (symbol-name hook) "-setup"))))
    `(add-hook ',hook (defun ,func-symbol (&rest args)
                        (when ,pred
                          (remove-hook ',hook ',func-symbol)
                          (message "Setting up %s" ,name)
                          ,@body)))))

(defmacro ii/eval-on-first-function (func name how pred &rest body)
  "For use with hooks that require `add-function'."
  (declare (indent defun))
  (let ((func-symbol (intern (concat "ii/" name "--on-first-" (symbol-name func) "-setup"))))
    `(add-function :after ,func (defun ,func-symbol (&rest args)
                                  (when ,pred
                                    (remove-function ,func ',func-symbol)
                                    (message "Setting up %s" ,name)
                                    ,@body)))))

(provide 'implicit-config-lib)
