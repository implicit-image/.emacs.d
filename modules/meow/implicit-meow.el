;;; -*- lexical-binding: t -*-

;;; Code:
(require 'meow)
(require 'implicit-meow-surround)
(require 'surround)
;; (require 'implicit-meow-beacon)

;;;; Variables

(defvar-local ii/meow--beacon-face-cookie nil)

(defvar ii/meow--toggled-case nil)

(defvar ii/meow--toggle-case-original-text nil)

;;;; Utility functions

(defun +meow--message (fstring type args)
  (let ((face (pcase type
                ('info 'success)
                ('warning 'warning)
                ('error 'error)
                (_ 'default))))
    (message (propertize (apply 'format `(,fstring ,@args))
                         'face face))))

(defun +meow--info (fstring &rest args)
  (+meow--message fstring 'info args))

(defun +meow--warning (fstring &rest args)
  (+meow--message fstring 'warning args))

(defun +meow--error (fstring &rest args)
  (+meow--message fstring 'error args))

(defun ii/meow--setup-mark-on-insert-change ()
  "Setup mark setting on next editing command."
  (add-hook 'post-self-insert-hook 'ii/meow--next-change-callback 0 t))

(defun ii/meow--next-change-callback ()
  "If the last mark was saved on different position than current, set mark."
  (let ((last-pos (car mark-ring)))
    (when (not (eq last-pos (point)))
      (push-mark))
    (remove-hook 'post-self-insert-hook 'ii/meow--next-change-callback t)))

(defun ii/meow--search-bounds (regex-p)
  "Return bounds for last regexp search."
  (isearch-forward regex-p)
  (let* ((rg (car (if regex-p
                      regexp-search-ring
                    search-ring)))
         (end (save-mark-and-excursion
                (isearch-end-of-buffer)
                (line-end-position)))
         (beg (save-mark-and-excursion
                (isearch-beginning-of-buffer)
                (line-beginning-position))))
    (cons beg end)))

(defun ii/meow--search-inner (regex-p)
  "Return bounds between last and next search result."
  (isearch-forward regex-p)
  (let* ((rg (car (if regex-p
                      regexp-search-ring
                    search-ring)))
         (beg (save-mark-and-excursion
                (or (isearch)
                    (point-min))))
         (end (save-mark-and-excursion
                (or (search-forward-regexp rg nil t 1)
                    (point-max)))))
    (isearch-done)
    (cons beg end)))

(defun ii/meow--regexp-search-inner ()
  (ii/meow--search-inner t))

(defun ii/meow--regexp-search-bounds ()
  (ii/meow--search-bounds t))

(defun ii/meow--string-search-inner ()
  (ii/meow--search-inner nil))

(defun ii/meow--string-search-bounds ()
  (ii/meow--search-bounds nil))

;;;; Commands

(defun backward-line (&optional n)
  (interactive "p")
  (forward-line (- (or n 1))))

(defun backward-symbol (&optional n)
  (interactive "p")
  (forward-symbol (- (or n 1))))

(defun +meow/yank (&optional save)
  (interactive "P")
  (when (region-active-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (if save
          (kill-region beg end)
        (delete-region beg end))))
  (meow-yank))

;; surround commands
(defun ii/meow-surround-with-char (char n)
  "Surround current selection with CHAR N times."
  (interactive (list
                (read-char "Surround with: ")
                (or prefix-arg nil)))
  (ii/meow--surround-with-char n (ii/meow--surround-get-matching-pair char)))

(defun ii/meow-delete-surrounding-char (char n)
  "Delete surrounding Nth character pair associated with CHAR ."
  (interactive (list
                (read-char "Delete surrounding: ")
                (or prefix-arg 1)))
  (ii/meow--delete-surrounding-chars n (ii/meow--surround-get-matching-pair char)))

(defun ii/meow-change-surrounding-char (char n)
  "Change surrounding Nth character pair associated with CHAR."
  (interactive (list
                (read-char "Change surrounding: ")
                (or prefix-arg 1)))
  (ii/meow--change-surrounding-chars n (ii/meow--surround-get-matching-pair char)))

(defun ii/meow-beacon-surround-with-char (char n)
  "Surround current selection with CHAR N times."
  (interactive (list
                (read-char "Surround with: ")
                (or prefix-arg nil)))
  (ii/meow--beacon-surround-with-char n (ii/meow--surround-get-matching-pair char)))

(defun ii/meow-beacon-delete-surrounding-char (char n)
  "Delete surrounding Nth character pair associated with CHAR ."
  (interactive (list
                (read-char "Delete surrounding: ")
                (or prefix-arg 1)))
  (ii/meow--beacon-delete-surrounding-chars n (ii/meow--surround-get-matching-pair char)))

(defun ii/meow-beacon-change-surrounding-char (char n)
  "Change surrounding Nth character pair associated with CHAR."
  (interactive (list
                (read-char "Change surrounding: ")
                (or prefix-arg 1)))
  (ii/meow--beacon-change-surrounding-chars n (ii/meow--surround-get-matching-pair char)))


(defun ii/meow-switch-char-case (&optional offset)
  "Toggle case of char at point."
  (interactive)
  (save-mark-and-excursion
    (let ((char (char-after (+ (point)
                               (or offset 0)))))
      (cond ((char-uppercase-p char)
             (delete-char 1)
             (insert-char (downcase char)))
            (t (delete-char 1)
               (insert-char (upcase char)))))))

(defun ii/meow--toggle-case-check ()
  ""
  (when (and (not (eq this-command 'ii/meow-switch-case))
             (not (eq this-command 'ii/meow-switch-char-case)))
    (setq-local ii/meow--toggle-case-original-text nil
                ii/meow--toggled-case nil)
    (remove-hook 'post-command-hook 'ii/meow--toggle-case-check t)))

(defun ii/meow--save-original-region-case (beg end)
  "temporarily save the contents of region from beg to end"
  (when (not (memq 'ii/meow--toggle-case-check post-command-hook))
    (add-hook 'post-command-hook 'ii/meow--toggle-case-check nil t))
  (when (null ii/meow--toggle-case-original-text)
    (let ((region (buffer-substring-no-properties beg end)))
      (setq-local ii/meow--toggle-case-original-text region))))

(defun ii/meow--reset-region-case (beg end)
  "Beg end."
  (when (and (region-active-p)
             (not (null ii/meow--toggle-case-original-text)))
    (replace-region-contents beg end ii/meow--toggle-case-original-text 0)))

(defun ii/meow-toggle-case-region (beg end)
  "Toggle the case of all characters from BEG to END."
  (interactive (list (region-beginning)
                     (region-end)))
  (save-mark-and-excursion
    (while (and (<= (point) end)
                (>= (point) beg))
      (let ((char (char-after)))
        (delete-char 1)
        (insert (funcall (if (char-uppercase-p char) 'downcase 'upcase) char))))))

(defun ii/meow-switch-case (&optional arg)
  (interactive "p")
  (meow--with-selection-fallback
   (save-mark-and-excursion
     (let ((beg (region-beginning))
           (end (region-end))
           (deactivate-mark (not arg)))
       (ii/meow--save-original-region-case beg end)
       (cond ((or (null ii/meow--toggled-case)
                  (eq ii/meow--toggled-case 'original))
              (setq-local ii/meow--toggled-case 'initials)
              (upcase-initials-region beg end))
             ((eq ii/meow--toggled-case 'initials)
              (setq-local ii/meow--toggled-case 'upcase)
              (upcase-region beg end))
             ((eq ii/meow--toggled-case 'upcase)
              (setq-local ii/meow--toggled-case 'downcase)
              (downcase-region beg end))
             ((eq ii/meow--toggled-case 'downcase)
              (setq-local ii/meow--toggled-case 'original)
              (ii/meow--reset-region-case beg end))
             (t (user-error "`ii/meow--toggled-case' not set properly")))))))

(defun ii/meow-upcase-dwim (arg)
  (interactive "P")
  (let ((deactivate-mark nil))
    (upcase-dwim (or arg 1))))

(defun ii/meow-downcase-dwim (arg)
  (interactive "P")
  (let ((deactivate-mark nil))
    (downcase-dwim (or arg 1))))

(defun ii/meow--beacon-mode-setup ()
  "Setup display settings for `meow-beacon-mode'."
  (if meow-beacon-mode
      (progn
        (let ((curr-bg (face-attribute 'region :background)))
          (setq-local ii/meow--beacon-face-cookie
                      (face-remap-add-relative 'region `(:background ,(doom-lighten curr-bg 0.1))))))
    (face-remap-remove-relative ii/meow--beacon-face-cookie)))

(defun ii/meow--change-number-at-point (change-by)
  "Change number at point by CHANGE-BY."
  (when-let* ((bounds (bounds-of-thing-at-point 'number))
              (beg (car bounds))
              (end (cdr bounds))
              (number (thing-at-point 'number)))
    (save-mark-and-excursion
      (replace-region-contents beg end (number-to-string (+ number change-by))))))

(defun ii/meow-increment-number-at-point (arg)
  (interactive "p")
  (ii/meow--change-number-at-point (or arg 1)))

(defun ii/meow-decrement-number-at-point (arg)
  (interactive "p")
  (ii/meow--change-number-at-point (- (or arg 1))))

(defun ii/meow-change-number-at-point (arg)
  "Change number at point by ARG, 1 by default."
  (interactive "p")
  (ii/meow--change-number-at-point (or arg 1)))

(defun ii/meow-edit-regexp-query ()
  "Edit last regexp query."
  (interactive)
  (let* ((rg (pop regexp-search-ring))
         (new-rg (read-string "Regexp:" rg 'regexp-history rg)))
    (if (not (string-blank-p new-rg))
        (push new-rg regexp-search-ring)
      (message "New regexp is blank"))))

(defun ii/meow-bounds-of-thing (thing)
  (interactive (list (meow-thing-prompt "Bounds of:")))
  (if (alist-get thing meow-char-thing-table)
      (meow-bounds-of-thing thing)
    (surround-mark-inner (char-to-string thing))))

(defun ii/meow-inner-of-thing (thing)
  (interactive (list (meow-thing-prompt "Inner of:")))
  (if (alist-get thing meow-char-thing-table)
      (meow-bounds-of-thing thing)
    (surround-mark-outer (char-to-string thing))))


;; (defun ii/meow-end-of-thing (thing arg)
;;   "Select up to end of THING. If ARG is positive, select additional ARG THINGs forward; if its negative select ARG THINGs backward."
;;   (interactive (list (meow-thing-prompt "End of:")
;;                      (or prefix-arg 0)))
;;   (cond ((not (meow-beacon-mode-p))
;;          (meow-end-of-thing thing)
;;          (ii/meow--adjust-direction arg)
;;          (ii/meow-expand-thing! thing arg))
;;         (t nil)))
;;
;; (defun ii/meow-beginning-of-thing (thing arg)
;;   (interactive (list (meow-thing-prompt "Beginning of:")
;;                      (or (numberp prefix-arg) 1)))
;;   (cond ((not (meow-beacon-mode-p))
;;          (meow-beginning-of-thing thing)
;;          (ii/meow--adjust-direction arg)
;;          (ii/meow-expand-thing! thing arg))
;;         (t nil)))
;;
;; (defun ii/meow-mark-word (n)
;;   "Select current word at point and following N - 1 words."
;;   (interactive (list (or prefix-arg 0)))
;;   (ii/meow--adjust-direction n)
;;   (meow-mark-word n)
;;   (ii/meow-expand-thing! 'word n))
;;
;; (defun ii/meow-prefix-arg-mode--turn-on ()
;;   "Turn on `meow-prefix-arg-mode'."
;;   (advice-add 'meow-bounds-of-thing :override 'ii/meow-bounds-of-thing)
;;   (advice-add 'meow-inner-of-thing :override 'ii/meow-inner-of-thing)
;;   (advice-add 'meow-beginning-of-thing :override 'ii/meow-beginning-of-thing)
;;   (advice-add 'meow-end-of-thing :override 'ii/meow-end-of-thing)
;;   (advice-add 'meow-mark-word :override 'ii/meow-mark-word)
;;   (advice-add 'meow--beacon-update-overlays :override 'ii/meow--beacon-update-overlays))
;;
;; (defun ii/meow-prefix-arg-mode--turn-off ()
;;   "turn off `meow-prefix-arg-mode'."
;;   (advice-remove 'meow-bounds-of-thing 'ii/meow-bounds-of-thing)
;;   (advice-remove 'meow-inner-of-thing 'ii/meow-inner-of-thing)
;;   (advice-remove 'meow-beginning-of-thing 'ii/meow-beginning-of-thing)
;;   (advice-remove 'meow-end-of-thing 'ii/meow-end-of-thing)
;;   (advice-remove 'meow-mark-word 'ii/meow-mark-word)
;;   (advice-remove 'meow--beacon-update-overlays 'ii/meow--beacon-update-overlays))
;;
;; (define-minor-mode ii/meow-prefix-arg-mode
;;   "Minor mode for enabling using prefix arguments with `meow' selection commands."
;;   (if (bound-and-true-p ii/meow-prefix-arg-mode)
;;       (ii/meow-prefix-arg-mode--turn-on)
;;     (ii/meow-prefix-arg-mode--turn-off)))

(defun ii/meow-indent-region-or-buffer (&optional arg)
  (interactive)
  (ii/window-stool--with-clear-buffer (current-buffer)
    (save-restriction
      (widen)
      (if (region-active-p)
          (indent-region (region-beginning) (region-end))
        (indent-region (point-min) (point-max))))))

(defun ii/meow-shell-command ()
  (interactive))

(defun ii/meow-pipe-to-shell-command ()
  (interactive))

(defun ii/meow-insert-shell-output ()
  (interactive))

(defun ii/meow-command (&optional arg)
  (interactive))

(provide 'implicit-meow)

