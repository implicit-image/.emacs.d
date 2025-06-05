(defun +mode-line--vc-update (&rest _)
  "Update modeline vc string."
  (setq +mode-line--vc-text
        (when (and (fboundp 'vc-root-dir)
                   (boundp 'vc-mode)
                   (vc-root-dir)
                   vc-mode
                   (buffer-file-name))
          (let* ((backend (vc-backend buffer-file-name))
                 (state (vc-state buffer-file-name backend))
                 (icon (cond ((memq state '(edited added)) "+")
                             ((eq state 'needs-merge) "^")
                             ((eq state 'needs-update) "v")
                             ((eq state 'removed) "-")
                             ((eq state 'conflict) "!")
                             ((eq state 'unregistered) "=")
                             (t "?")))
                 (face (pcase icon
                         ("+" 'success)
                         ("^" 'success)
                         ("v" 'warning)
                         ("-" 'error)
                         ("!" 'error)
                         ("=" 'vc-dir-status-ignored)
                         (_ 'vc-dir-status-ignored))))
            (concat (propertize icon 'face face)
                    " "
                    (if (eq icon "?")
                        "none"
                      (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))))))
  (force-mode-line-update))

(provide 'init-vc-impl)
