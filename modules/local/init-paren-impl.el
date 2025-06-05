;;;###autoload
(defun +paren--overlay-function (text)
  "Show TEXT in an overlay at the top-left of the current window with nice face spec."
  (setq text (replace-regexp-in-string "\n" " " text))
  (show-paren--delete-context-overlay)
  (let* ((beg (window-start))
         (end (save-excursion
                (goto-char beg)
                (line-end-position))))
    (setq show-paren--context-overlay (make-overlay beg end)))
  (overlay-put show-paren--context-overlay 'display text)
  ;; Use the (default very high) `show-paren-priority' ensuring that
  ;; not other overlays shine through (bug#59527).
  (overlay-put show-paren--context-overlay 'priority
               show-paren-priority)
  (overlay-put show-paren--context-overlay
               'face `(:box
                       ( :line-width (1 . -1)
                         :color ,(face-attribute 'shadow :foreground))
                       :foreground ,(doom-color 'red)))
  (add-hook 'post-command-hook #'show-paren--delete-context-overlay
            nil 'local))

(provide 'init-paren-impl)
