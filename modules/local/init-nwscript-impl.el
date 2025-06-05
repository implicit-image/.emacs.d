;;;###autoload
(defun +nwscript-mode--setup ()
  (setq-local case-fold-search t)
  (add-hook 'completion-at-point-functions (cape-capf-super (cape-company-to-capf 'company-dabbrev-code)
                                                            'cape-file)
            nil t)
  (funcall-interactively 'untabify (point-min) (point-max))
  (save-buffer)
  (when indent-tabs-mode
    (indent-tabs-mode -1)))


(provide 'init-nwscript-impl)
