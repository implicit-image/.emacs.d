(require 'popup)

;;;; Alists of lookup functions
;;;; each list contains (MAJOR-MODE . LOOKUP-FUNCTION) pairs
;;;; if current mode is not found, the default lookup mechanism, `+lookup/' is used

;;;###autoload
(defun +lookup/definition ()
  (interactive)
  (message "not implemented yet!"))

;;;###autoload
(defun +lookup/references ()
  (interactive)
  (message "not implemented yet!"))

;;;###autoload
(defun +lookup/implementation ()
  (interactive)
  (message "not implemented yet!"))

;;;###autoload
(defun +lookup/web ()
  (interactive)
  (message "not implemented yet!"))

;;;###autoload
(defun +lookup/documentation (&optional popup-window)
  "Lookup documentation for symbol at point. If POPUP-WINDOW is non-nil, \
immediately show documentation popup window, else try showing in-window first."
  (interactive "P")
  (let ((buffer-lookup-function (alist-get major-mode +lookup/buffer-functions-alist))
        (popup-lookup-function (alist-get major-mode +lookup/popup-functions-alist)))
    (cond ((and popup-lookup-function
                (display-graphic-p))
           (funcall-interactively popup-lookup-function))
          (buffer-lookup-function
           (funcall-interactively buffer-lookup-function))
          ((or (bound-and-true-p eldoc-box-hover-at-point-mode)
               (bound-and-true-p eldoc-box-hover-mode))
           (eldoc-box-help-at-point))
          ((memq 'lsp-ui-mode local-minor-modes)
           (if (display-graphic-p)
               (lsp-ui-doc-glance)
             (lsp-describe-thing-at-point)))
          ((bound-and-true-p lspce-mode) (lspce-help-at-point))
          (t (message "No documentation function found")))))

(provide 'init-lookup-impl)
