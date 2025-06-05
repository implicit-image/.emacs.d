;;; -*- lexical-binding: t -*-

(defvar-local +indent-tab-function nil)

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

(defmacro +set-tab-function! (mode function &optional hook)
  "Set default tab function FUNCTION for MODE in HOOK."
  (let ((fun (intern (concat "+" (symbol-name mode) "-indent-setup")))
        (hook (or (when (bound-and-true-p hook) hook)
                  (intern (concat (symbol-name mode) "-hook")))))
    `(progn (defun ,fun ()
              (setq-local +indent-tab-function ',function))
            (add-hook (quote ,hook) (quote ,fun)))))

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

(defmacro +when-idle! (min-time &rest body)
  "Run body with next idle timer."
  (declare (indent defun))
  `(run-with-idle-timer ,min-time nil (lambda () ,@body)))


(defun +char-whitespace? (char)
  "Check if CHAR is whitespace."
  (or (eq char 32)
      (eq char 160)
      (eq char 9)))

(defun +os/is-windows-p ()
  (eq system-type 'windows-nt))

(defun +os/is-linux-p ()
  (eq system-type 'gnu/linux))

(defun +os/is-wsl-p ()
  (and (+os/is-linux-p)
       (file-exists-p "/proc/sys/fs/binfmt_misc/WSLInterop")))

(when (+os/is-windows-p)
  (add-to-list 'exec-path "c:/ProgramData/chocolatey/bin/")
  (add-to-list 'exec-path "c:/ProgramData/mingw64/mingw64/bin/")
  (add-to-list 'exec-path "c:/Program Files/Git/cmd/")
  (add-to-list 'exec-path "c:/Program Files/Git/usr/bin/")
  (add-to-list 'exec-path "c:/Users/b/AppData/Local/Programs/MiKTeX/miktex/bin/x64")
  (prefer-coding-system 'utf-8))

(when (+os/is-wsl-p)
  (add-to-list 'exec-path "/mnt/c/Program Files/Mozilla Firefox/"))


(provide 'init-config-lib)
