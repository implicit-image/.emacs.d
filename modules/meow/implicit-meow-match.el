;;; -*- lexical-binding: t -*-
;; implementation for match and surround functionality
;; in style of helix


(require 'implicit-utils)
(require 'meow)

(eval-when-compile
  (require 'mc-mark-more))

(defvar ii/meow--match-functions
  '((?s . ("char" . +meow-surround-with-char))
    (?S . ("str" . +meow-surround-with-string))
    (?d . ("remove char" . +meow-delete-surrounding-chars))
    (?D . ("remove str" . +meow-delete-surrounding-string))
    (?r . ("replace char" . +meow-replace-surrounding-char))
    (?R . ("replace str" . +meow-replace-surrounding-string))
    (?m . ("pair" . +meow-mark-matching)))
  "TODO.")

(defvar ii/meow-surround-mode-specific-pair-alist nil
  "Mapping from major mode to mode-specific surround pairs.")

(defvar-local ii/meow-surround-local-char-pairs nil
  "Buffer local char pair for surrounding.")

(defvar-local ii/meow-surround-local-string-pairs nil
  "Buffer local string pair templates for surrounding.")

(defmacro ii/meow-define-pair (&rest args)
  ""
  (let ((local (plist-get args :mode)))
    (if (not local)
        `(progn
           (add-to-list insert-pair-alist (,first ,second)))
      `(progn
         (add-to-list ii/meow-surround-mode-specific-pair-alist )))))

(defun ii/meow--format-surround-template-string (str)
  "Format STR ")

(defun ii/meow--match-prompt ()
  "Return a prompt for `+meow/match'."
  (ii/make-key-prompt "Match: " ii/meow--match-functions))

(defun +meow--surround-get-matching-pair (char)
  "Get matching pair for CHAR."
  (cl-reduce (lambda (_ac _el)
               (if (memq char _ac) _ac _el))
             insert-pair-alist))

(defun +meow--find-surrounding-chars (open close &optional nth)
  (let* ((nth (or nth 1))
         (prev (save-mark-and-excursion
                 (search-backward open nil nil nth)))
         (next (save-mark-and-excursion
                 (search-forward close nil nil nth))))
    (cons prev next)))

(defun +meow--surround-with-char (arg open close)
  (let ((parens-require-spaces nil))
    (save-mark-and-excursion
      (insert-pair nil open close))
    (when arg
      (save-mark-and-excursion
        (let* ((beg (region-beginning))
               (end (region-end))
               (char-start (1+ beg))
               (char-end (1+ end)))
          ;; point is always BEFORE the character
          (goto-char char-start)
          ;; reset point position to the start of the expression
          (set-mark (point))
          (insert ?\n)
          (goto-char (- end 1))
          (insert 32)
          (backward-char)
          (insert ?\n)
          (forward-char)
          (indent-region (region-beginning)
                         (region-end)))))))

(defun +meow--delete-surrounding-chars (arg open close)
  "ARG OPEN CLOSE."
  (let* ((bounds (+meow--find-surrounding-chars open close arg))
         (open-pos (car bounds))
         (close-pos (cdr bounds)))
    (save-mark-and-excursion
      (goto-char open-pos)
      (delete-char)
      (goto-char close-pos)
      (delete-char))))

(defun +meow-delete-surrounding-chars (&optional with-newline)
  "Delete WITH-NEWLINE."
  (let* ((chars (+meow--surround-get-matching-pair (read-char "Delete surrounding: ")))
         (open (car chars))
         (close (cdr chars)))
    (if (and open close)
        (+meow--delete-surrounding-chars with-newline
                                         open
                                         close)
      (user-error "Failed determining matching characters"))))

(defun +meow-surround-with-char (&optional with-newline)
  "Surround a thing with provided character. If prefix argument WITH-NEWLINE\
is non-nil, insert newline after the opening character and before the closing\
one, indenting the region afterwards."
  (when (not (region-active-p))
    (meow-inner-of-thing (meow-thing-prompt "Surround: ")))
  (let* ((chars (+meow--surround-get-matching-pair (read-char "Surround with: ")))
         (open (car chars))
         (close (cadr chars)))
    (if (and open close)
        (+meow--surround-with-char with-newline
                                   open
                                   close)
      (user-error "Failed determining matching characters"))))

(defun +meow-surround-with-string (&optional with-newline)
  (when (not (region-active-p))
    (meow-inner-of-thing (meow-thing-prompt "Surround: ")))
  (let* ((string (read-string "")))
    (+meow--surround-with)))

(provide 'implicit-meow-match)
