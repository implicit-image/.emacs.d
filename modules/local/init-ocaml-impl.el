
;;;###autoload
(defun +merlin-mode--setup ()
  (setq-local eldoc-echo-area-use-multiline-p t
              merlin-eldoc-max-lines 10
              merlin-eldoc-delimiter " \n "))

(provide 'init-ocaml-impl)
