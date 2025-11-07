;;; -*- lexical-binding: t -*-


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
     (-zip-pair (number-sequence 0 (length actions))
                actions))))

(defun +lsp--lookup-docs ()
  (interactive)
  (if (display-graphic-p)
      (lsp-ui-doc-glance)
    (lsp-describe-thing-at-point)))

(defun +lsp--setup ()
  (setq-local +lookup-documentation-function '+lsp--lookup-docs))

(defun +lspce--setup ()
  (setq-local +lookup-documentation-function 'lspce-help-at-point))

;;;###autoload
(defun +lsp-apply-code-action-in-popup (&optional select-kind)
  (interactive "P")
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
    (if (length> maybe-action 0)
        (let ((action (car maybe-action)))
          (map-delete action :index)
          (lsp-execute-code-action (car maybe-action))))))

(provide 'implicit-lsp)
