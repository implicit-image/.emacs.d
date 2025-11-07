;;; -*- lexical-binding: t -*-

(require 'transient)

(defmacro +transient/define-infixes! (&rest args)
  "Helper macro to define many simple `transient' infixes."
  (declare (indent defun))
  `(progn ,@(mapcar (lambda (infix-args)
                      `(transient-define-infix ,(car infix-args) nil
                         ,@(cdr infix-args)))
                    args)))


(provide 'implicit-transient-utils)
