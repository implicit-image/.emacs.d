(require 'lsp-mode)
(require 'popup)


(defvar-local popup--code-action-index nil)

(defun +lsp--get-annotated-actions-at-point (&optional kind)
  (let ((actions (lsp-code-actions-at-point)))
    (mapcar
     (lambda (cell)
       (let ((num (car cell))
             (l (cdr cell)))
         (plist-put l :index num)
         l))
     (-zip (number-sequence 0 (length actions))
           actions))))

;;;###autoload
(defun +lsp-apply-code-action-in-popup (&optional select-kind)
  (interactive "P")
  (when +eldoc--display-popup?
    (setq-local +eldoc--display-popup? nil))
  (let* ((actions (+lsp--get-annotated-actions-at-point))
         (names (mapcar (lambda (action)
                          (propertize (lsp:code-action-title action)
                                      :index (plist-get action :index)))
                        actions))
         (selected (popup-menu* names
                                :around t
                                :max-width (* 0.8 (window-width))))
         (maybe-action (seq-filter (lambda (action)
                                     (eq (plist-get action :index)
                                         (get-text-property 0 :index selected)))
                                   actions)))
    (print maybe-action)
    (if (length> maybe-action 0)
        (let ((action (car maybe-action)))
          (map-delete action :index)
          (lsp-execute-code-action (car maybe-action))
          (setq-local +eldoc--display-popup? t)))))

(provide 'init-lsp-mode-impl)
