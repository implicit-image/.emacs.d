;;; -*- lexical-binding: t -*-
;; Improved selection manipulation

(require 'meow)

(defvar ii/meow-beacon-update-function-alist nil
  "Alist of form (SELECTION-TYPE . FUNCTION).")

(defmacro ii/meow-expand-thing! (thing arg)
  `(progn
     (ii/meow--adjust-direction ,arg)
     (when (and (numberp ,arg) (not (eq ,arg 0)))
       (ii/meow-expand-thing ,thing (abs ,arg)))))

(defun ii/meow-expand-thig (thing n)
  "Expand current selection based on THING N times."
  (let ((thing (if (characterp thing)
                   (cdr (assq thing meow-char-thing-table))
                 thing)))
    (pcase thing
      ('line (meow-line n))
      ('symbol (meow-next-symbol n))
      (_ (meow-expand n)))))

(defun ii/meow--adjust-direction (n)
  "Call `meow-reverse' if numeric prefix arg N is negative and there is a region active."
  (when (and (region-active-p)
             (< n 0))
    (meow-reverse)))

(defun ii/meow--apply-to-isearch-matches ())

(defun ii/meow--beacon-update-overlays ()
  "Update overlays for BEACON state."
  (meow--beacon-remove-overlays)
  (when (meow--beacon-inside-secondary-selection)
    (let* ((ex (car (meow--selection-type)))
           (type (cdr (meow--selection-type))))
      (cl-case type
        ((nil transient) (meow--add-beacons-for-char))
        ((word) (if (not (eq 'expand ex))
                    (meow--add-beacons-for-thing meow-word-thing)
                  (meow--add-beacons-for-match (meow--beacon-region-words-to-match))))
        ((symbol) (if (not (eq 'expand ex))
                      (meow--add-beacons-for-thing meow-symbol-thing)
                    (meow--add-beacons-for-match (meow--beacon-region-words-to-match))))
        ((visit) (meow--add-beacons-for-match (car regexp-search-ring)))
        ((line) (meow--add-beacons-for-line))
        ((join) (meow--add-beacons-for-join))
        ((find) (meow--add-beacons-for-find))
        ((till) (meow--add-beacons-for-till))
        ((char) (when (eq 'expand ex) (meow--add-beacons-for-char-expand)))))))

(provide 'implicit-meow-beacon)

