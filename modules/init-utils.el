;;; -*- lexical-binding: t -*-

(defun +utils/toggle-mode (mode)
  (interactive)
  (if (symbol-value mode)
      (funcall-interactively mode -1)
    (funcall-interactively mode -1)))

(defun +utils-nth-wrapped (n list)
  "Returnd nth element of `list'. If `n' is greater than length of `list' takes `(mod n (length list))' instead."
  (nth (mod (length list) n) list))

;; (defun +utils/open-in-chromium (&optional thing-at-point)
;;   (interactive)
;;   (let ((url (if thing-at-point
;; 		 (thing-at-point-url-at-point)
;; 	       ()))))

;;;###autoload
(defun +utils/delete-visited-file ()
  "Delete the file visited by current buffer."
  (interactive)
  (let* ((curr-buf (current-buffer))
	 (curr-name (buffer-file-name curr-buf)))
    (when (yes-or-no-p (string-join `("Delete " ,curr-name " file?")))
      (kill-buffer curr-buf)
      (delete-file curr-name))))

;;;###autoload
(defun +utils/consult-set-font-family ()
  "Set font family in all frames to selected one."
  (interactive)
  (if (display-graphic-p)
      (let ((fg-color (doom-color 'strings))
	    (bg-color (doom-color 'bg)))
	(consult--read (delete-dups (font-family-list))
		       :prompt "Font family: "
		       :annotate (lambda (font)
				   `(,(propertize
				       (concat font " ")
				       'face 'font-lock-keyword-face)
				     "Family: "
				     ,(propertize
				       "The quick brown fox jumps over the lazy dog."
				       'face `(:family ,font :foreground ,fg-color :background ,bg-color))))
		       :require-match t
		       :history 'consult-set-font-family-history
		       :lookup (lambda (selected-font &rest args)
				 (interactive)
				 (set-frame-font selected-font t t t))))
    (message "Cant change font family on tty")))

(defun +utils-whole-buffer-as-string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(use-package restart-emacs
  :general
  (+leader-keys
    "q r" '("Restart emacs" . restart-emacs)))

(use-package sideline)

(use-package ready-player
  :hook
  (after-init . ready-player-mode))

(+leader-keys
  "f D" '("Delete current file" . +utils/delete-visited-file)
  "q A" '("Save all and kill emacs" . save-buffers-kill-emacs)
  "t c" '("Colorize color strings" . rainbow-mode)
  "t I" '("Select input method" . set-input-method)
  "t v" '("Visual line mode" . visual-line-mode))

(provide 'init-utils)
