(require 'seq)

(defmacro +mapcar-non-nil! (function sequence)
  "Map a FUNCTION over a SEQUENCE and return a list of results with only non-nil values."
  `(seq-filter (lambda (e)
                 e)
               (seq-map ,function
                        ,sequence)))

(provide 'implicit-macros)
