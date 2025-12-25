;; -*- lexical-binding: t; -*-

(require 'meow)

(defmacro ii/meow--apply-to-beacon-overlays! (beg-sym end-sym &rest expr)
  (declare (indent defun))
  `(meow--wrap-collapse-undo
     (let ((,beg-sym (region-beginning))
           (,end-sym (region-end)))
       ,@expr
       (save-mark-and-excursion
         (cl-loop for ov in meow--beacon-overlays do
                  (when (and (overlayp ov)
                             (not (eq 'cursor (overlay-get ov 'meow-beacon-type))))
                    (let ((,beg-sym (overlay-start ov))
                          (,end-sym (overlay-end ov)))
                      (goto-char ,beg-sym)
                      (push-mark ,end-sym t)
                      ,@expr
                      (delete-overlay ov))))))))

(provide 'implicit-meow-utils)
