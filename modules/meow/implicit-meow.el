;;; -*- lexical-binding: t -*-


;;; Code:
(require 'meow)
(require 'implicit-meow-match)
(require 'implicit-meow-command)


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
    (isearch-done)
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

(defun ii/meow-next-defun (arg)
  (beginning-of-defun arg)
  (point))

(defun ii/meow-next-line (arg)
  (beginning-of-line arg)
  (point))

(defun ii/meow-next-paragraph (arg)
  (forward-paragraph arg)
  (point))

(defun ii/meow-next-sentence (arg)
  (forward-sentence arg)
  (point))


;;;; Commands

(defun backward-line (&optional n)
  (interactive "p")
  (forward-line (- (or n 1))))

(defun backward-symbol (&optional n)
  (interactive "p")
  (forward-symbol (- (or n 1))))

(defun +meow--beacon-advice ()
  (setq meow--beacon-defining-kbd-macro 'record))

(defun +meow/yank (&optional save)
  (interactive "P")
  (when (region-active-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (if save
          (kill-region beg end)
        (delete-region beg end))))
  (meow-yank))

(defun +meow/insert-exit ()
  "Switch to NORMAL state."
  (interactive)
  (cond
   ((meow-keypad-mode-p)
    (meow--exit-keypad-state))
   ((and (meow-insert-mode-p)
         (or (eq meow--beacon-defining-kbd-macro 'quick)
             (eq meow--beacon-defining-kbd-macro 'record)))
    (setq meow--beacon-defining-kbd-macro nil)
    (meow-beacon-insert-exit))
   ((meow-insert-mode-p)
    (meow--switch-state 'normal))))

(defun +meow/grab ()
  (interactive)
  (save-mark-and-excursion
    (when (not (or (region-active-p)
                   meow-beacon-mode))
      (meow-bounds-of-thing (meow-thing-prompt "Grab: ")))
    (meow-grab)))

(defun +meow/beacon-on-symbol ()
  (interactive)
  (save-mark-and-excursion
    (when (not (region-active-p))
      (meow-bounds-of-thing (meow-thing-prompt "Surround: "))
      (meow-grab)))
  (meow-mark-symbol 1))

(defun ii/meow-surround-with-char ())

(defun ii/meow-surround-with-string ())

(defun ii/meow-delete-surrounding-char ())

(defun ii/meow-delete-surrounding-string ())

(defun +meow/match (where &optional arg)
  ""
  (interactive (list (read-char-from-minibuffer
                      (ii/meow--match-prompt)
                      (mapcar (lambda (cell)
                                (car cell))
                              ii/meow--match-functions))
                     current-prefix-arg))
  (if-let ((fn (cdr (alist-get where ii/meow--match-functions))))
      (funcall fn arg)
    (message "Operation %c not recognized" where)))

(defvar ii/meow--toggled-case nil)
(defvar ii/meow--toggle-case-original-text nil)

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
    (upcase-dwim arg)))

(defun ii/meow-downcase-dwim (arg)
  (interactive "P")
  (let ((deactivate-mark nil))
    (downcase-dwim arg)))

(defvar-local ii/meow--beacon-face-cookie nil)

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

;; (defvar ii/meow-next-thing-function-alist nil
;;   "Alist of `meow' THING and functions to find next occurrence of THING.")
;;
;; (defun ii/meow-next-thing (thing &optional arg)
;;   "THING."
;;   (interactive (list (meow-thing-prompt "next")
;;                      prefix-arg))
;;   (when-let* ((fn (alist-get thing ii/meow-next-thing-function-alist)))
;;     (meow-cancel-selection)
;;     (funcall-interactively fn arg)))
;;
;; (defvar-local ii/meow--last-selected-thing nil
;;   "Stores last thing selected from `meow-thing-prompt'.")
;;
;; (defun ii/meow--thing-prompt-advice (thing)
;;   (setq-local ii/meow--last-selected-place this-command
;;               ii/meow--last-selected-thing thing))
;;
;; (advice-add 'meow-thing-prompt :filter-return 'ii/meow--thing-prompt-advice)

(provide 'implicit-meow)



