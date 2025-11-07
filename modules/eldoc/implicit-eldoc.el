

;;; Code:


(defvar ii/eldoc--frame-default-parameters
  '((name . "*eldoc-frame*")
    (minibuffer . nil)
    (width . 20)
    (height . 3))
  "Default aprameters for eldoc child frame.")

(defun ii/eldoc-make-frame ()
  "Make eldoc frame."
  (let ((buf (eldoc-doc-buffer nil)))
    (when buf
      (with-current-buffer buf
        (make-frame ii/eldoc--frame-default-parameters)))))

(defun ii/eldoc--buffer-function (docs interactive)
  "DOCS INTERACTIVE."
  (if (not (null docs))
      (eldoc-doc-buffer t)
    (quit-window nil (get-buffer-window "\*eldoc\*" (selected-frame)))))

(defun ii/eldoc--frame-function (docs interactive)
  "DOCS INTERACTIVE."
  (if (not (null docs))
      (ii/eldoc-make-frame)))

(provide 'implicit-eldoc)
