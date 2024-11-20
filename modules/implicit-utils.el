

(defun +utils/toggle-mode (mode)
  (interactive)
  (if (symbol-value mode)
      (funcall-interactively mode -1)
    (funcall-interactively mode -1)))


(defun +utils/counsel-toggle-minor-mode ()
  "Toggle the selected minor mode"
  (interactive)
  (ivy-read "Minor mode: " (delete-dups minor-mode-list)
	    :preselect 0
	    :require-match t
	    :history 'counsel-toggle-minor-mode-history
	    :action (lambda (minor-mode)
		      (interactive)
		      (let ((minor-mode-var (intern minor-mode)))
			(if (symbol-value minor-mode-var)
			    (funcall-interactively minor-mode-var -1)
			  (funcall-interactively minor-mode-var +1))))
	    :caller '+utils/counsel-toggle-minor-mode))


(defun +utils-nth-wrapped (n list)
  "Returnd nth element of `list'. If `n' is greater than length of `list' takes `(mod n (length list))' instead."
  (nth (mod (length list) n) list))


;;;###autoload
(defun +utils/delete-visited-file ()
  (interactive)
  (let* ((curr-buf (current-buffer))
	 (curr-name (buffer-file-name curr-buf)))
    (when (yes-or-no-p (string-join `("Delete " ,curr-name " file?")))
      (kill-buffer curr-buf)
      (delete-file curr-name))))

;;;###autoload
(defun +utils/counsel-set-font ()
  (interactive)
  (let ((current-font
         (symbol-name (font-get (face-attribute 'default :font) :family)))
	(current-font-size
	 (font-get (face-attribute 'default :font) :size)))
    (ivy-read "Font: " (delete-dups (font-family-list))
              :preselect current-font
              :require-match t
              :history 'counsel-set-font-history
              :action (lambda (selected-font)
			(interactive)
			(set-frame-font selected-font t t t))
              :caller 'counsel-set-font)))



(defun +utils-whole-buffer-as-string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(use-package restart-emacs)

(use-package sideline)

(provide 'init-utils)
