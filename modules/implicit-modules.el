;;; atm not used
;; TODO figure out what to do with this


(defun +modules/require (modname)
  (let ((mod-dir-path (concat my/modules-dir-path (symbol-name modname)))
	(submod-path (concat mod-dir-path "/" (symbol-name submodname) ".el")))
    (add-to-list 'load-path mod-dir-path)
    (require submodname submod-path)))

(defun +modules/browse ()
  (interactive)
  (counsel-find-file "" +modules/path))

(provide 'implicit-modules)
