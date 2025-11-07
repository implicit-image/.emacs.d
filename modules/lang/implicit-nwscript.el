;;; -*- lexical-binding: t -*-

(defvar-local +nwscript--project-root nil
  "")

(defvar-local +nwscript--include-dirs '("include/"))



(defun +nwscript--setup ()
  (setq-local case-fold-search t)
  (add-hook 'completion-at-point-functions
            (cape-capf-super (cape-company-to-capf 'company-dabbrev-code)
                             'cape-file)
            nil t)
  (add-hook 'completion-at-point-functions '+nwscript--completion-at-point nil t)
  (funcall-interactively 'untabify (point-min) (point-max))
  (save-buffer))

(provide 'implicit-nwscript)
