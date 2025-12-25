;; -*- lexical-binding: t; -*-

(require 'meow)
(require 'implicit-meow-utils)

(defvar ii/meow-next-thing-function-alist nil
  "Alist of `meow' things and functions for finding their next occurrence."  )

(defvar ii/meow-next-thing-registry nil
  "TODO")

(defmacro ii/meow-thing-register-extended! (thing inner bounds next &optional prev)
  "Register a THING with INNER, BOUNDS, NEXT and PREV."
  `(progn
     (when (not (plist-get meow--thing-registry ,thing))
       (meow-thing-register ,thing ,inner ,bounds))
     (ii/meow-register-next-thing-function ,thing ,next ,prev)))

(defun ii/meow--search-regexp-in-direction (regexp direction count)
  ""
  (funcall
   (pcase direction
     ('forward 're-search-forward)
     ('backward 're-search-backward))
   regexp nil t count))

(defun ii/meow--skip-syntax-in-direction (syntax direction count)
  (funcall
   (pcase direction
     ('forward 'skip-syntax-forward)
     ('backward 'skip-syntax-backward))
   syntax))

(defun ii/meow--next-thing-syntax-function (syntax arg)
  (save-mark-and-excursion
    ()))

(defun ii/meow--next-thing-regexp-function (regexp arg same)
  (let ((arg (if same (* arg 2) arg))
        (direction ()))
    (list
     (save-mark-and-excursion
       ii/meow--search-regexp-in-direction regexp direction arg)
     (save-mark-and-excursion
       ii/meow--search-regexp-in-direction regexp direction arg))))

(defun ii/meow--next-thing-pair-function ())

(defun ii/meow--next-thing-pair-regexp-function ())

(defun ii/meow-register-next-thing-function (thing forward-fn &optional backward-fn)
  "Register FORWARD-FN as next thing function for THING. FORWARD-FN should accept negative argument to go backwards instead. If it doesn't, BACKWARD-FN should be provided instead. Both functions receive no arguments and return the bounds of the next occurrence of THING, or nil if none are found in the buffer."
  (cl-flet ((parse-function (fn)
              (cond ((functionp fn) fn)
                    ((equal 'syntax (car fn))
                     (lambda (arg) (ii/meow--next-thing-syntax-function (cadr fn) arg)))
                    ((equal 'regexp (car fn))
                     (lambda (arg) (ii/meow--next-thing-regexp-function arg)))
                    ((equal 'pair (car fn))
                     (lambda (arg) (ii/meow--next-thing-pair-function arg)))
                    ((equal 'pair-regexp (car fn))
                     (lambda (arg) (ii/meow--next-thing-pair-regexp-function arg))))))
    (let* ((next-fn (parse-function forward-fn))
           (prev-fn (parse-function (or backward-fn forward-fn))))
      (setf (alist-get thing ii/meow-next-thing-registry) (list forward-fn (or backward-fn
                                                                               forward-fn))))))

(defun ii/meow--select-bounds (bounds)
  (let (())
    (meow--select-range)))

(defun ii/meow-get-next-thing-function (thing direction)
  (pcase direction
    ('forward (alist-get thing ii/meow-next-thing-registry))
    ('backward (alist-get thing ii/meow-next-thing-registry))))

(defun ii/meow-next-line (n)
  "Return bounds of Nth line. Searches backwards if n is negative."
  (save-mark-and-excursion
    (forward-line n)
    (bounds-of-thing-at-point 'line)))

(defun ii/meow-next-visual-line (n)
  (save-mark-and-excursion
    (line-move-visual n)
    (meow--inner-of-visual-line)))

(defun ii/meow-next-sentence (n)
  (save-mark-and-excursion
    (forward-sentence n)
    (bounds-of-thing-at-point 'sentence)))

(defun ii/meow-next-paragraph (n)
  (save-mark-and-excursion
    (forward-paragraph n)
    (bounds-of-thing-at-point 'paragraph)))

(defun ii/meow--in-bounds-p (pos bounds)
  (and bounds
       (< pos (car bounds))
       (> pos (cdr bounds))))

(defun ii/meow-next-window (&optional n)
  (let ((og-start (window-start))
        (og-end (window-end))
        (window-lines (count-lines (window-start) (window-end)))
        (next-start (if n )))
    (save-mark-and-excursion
      (set-window-start (selected-window) (1+ og-end))
      (prog1 (cons (window-start) (window-end))
        (set-window-start (selected-window) og-start)))))

(defun ii/meow-next-defun (n)
  (save-mark-and-excursion
    (beginning-of-defun n)
    (bounds-of-thing-at-point 'defun)))

(defun ii/meow-next-string (n)
  (save-mark-and-excursion
    (let ((bounds (bounds-of-thing-at-point 'string)))
      (if (not (ii/meow--in-bounds-p (point) bounds))
          (ii/meow--skip-syntax-in-direction "")))))

(defun ii/meow--get-current-direction ()
  (if (>= (point) (mark t))
      'forward
    'backward))

(defun ii/meow--adjust-amount (n)
  (pcase (ii/meow--get-current-direction)
    ('forward (if (<= n 0) 1 n))
    ('backward (if (>= n 0) -1 n))
    (_ n)))

(defun ii/meow-expand-to-next-thing (thing n))

(defun ii/meow-select-nth-thing (thing))

(defun ii/meow-beacon-expand-to-next-thing ())

(defun ii/meow-beacon-select-nth-thing ())

(defun ii/meow--next-thing-register-defaults ()
  "Register default `meow-next-thing' functions for built-in `meow' things."
  (ii/meow-register-next-thing-function 'defun 'ii/meow-next-defun)
  (ii/meow-register-next-thing-function 'line 'ii/meow-next-line)
  (ii/meow-register-next-thing-function 'paragraph ii/meow-next-paragraph)
  (ii/meow-register-next-thing-function 'round '(pair ("(") (")")))
  (ii/meow-register-next-thing-function 'square '(pair ("[") ("]")))
  (ii/meow-register-next-thing-function 'curly '(pair ("{") ("}")))
  (ii/meow-register-next-thing-function 'sentence 'ii/meow-next-sentence)
  (ii/meow-register-next-thing-function 'string 'ii/meow-next-string)
  (ii/meow-register-next-thing-function 'word )
  (ii/meow-register-next-thing-function 'symbol)
  (ii/meow-register-next-thing-function 'visual-line 'ii/meow-next-visual-line)
  (ii/meow-register-next-thing-function 'window 'ii/meow-next-window))

(defun ii/meow--next-thing-register-treesit-defaults ()
  (with-eval-after-load 'meow-tree-sitter
    (ii/meow-register-next-thing)))

(provide 'implicit-meow-next-thing)


