;;; -*- lexical-binding: t -*-
;; Improved selection manipulation

(require 'meow)

(eval-when-compile
  (require 'mc-mark-more))

(defvar +meow-select-fn-thing-alist '((meow-mark-word . word)
                                      (meow-mark-symbol . symbol)
                                      (meow-line . line)))

(defvar +meow--current-selection-type nil
  "Type of current selection.")

(defun +meow--cancel-selection-advice ()
  (setq-local +meow--current-selection-type nil))

(defun +meow--select-advice (select-fn &rest args)
  (setq-local +meow--current-selection-type
              (alist-get select-fn +meow-select-fn-thing-alist))
  (command-execute select-fn t))

(defun +meow--x-of-thing-advice (x-of-thing-fn &rest args)
  (let ((thing-char (meow-thing-prompt (pcase x-of-thing-fn
                                         ('meow-inner-of-thing "Inner of:")
                                         ('meow-bounds-of-thing "Bounds of:")
                                         ('meow-beginning-of-thing "Beginning of:")
                                         ('meow-end-of-thing "End of:")
                                         (_ nil))))
        (thing (pcase thing-char
                 (?\d 'defun)
                 (?\. 'sentence)
                 (?\p 'paragraph)
                 (?\w 'window)
                 (?\s 'string)
                 (_ 'transient))))
    (setq-local +meow--current-selection-type thing)
    (funcall-interactively x-of-thing-fn thing)))

(provide 'implicit-meow-selection)
