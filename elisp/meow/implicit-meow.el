;;; -*- lexical-binding: t -*-

;;; Code:
(require 'meow)
(require 'surround)
(require 'implicit-meow-surround)
(require 'implicit-ui)
;; (require 'implicit-meow-thing)
;; (require 'implicit-meow-beacon)


;; multilpe-cursors functions
(declare-function mc/remove-fake-cursors "multiple-cursors")
(declare-function mc/create-fake-cursor-at-point "multiple-cursors")
(declare-function mc/num-cursors "multiple-cursors")
(declare-function mc/disable-multiple-cursors-mode "multiple-cursors")
(declare-function mc/pop-state-from-overlay "multiple-cursors")

;; iedit functions
(declare-function iedit-mode "iedit-mode")
(declare-function iedit-start "iedit-mode")
(declare-function iedit-counter "iedit-mode")
(declare-function iedit-goto-first-occurrence "iedit-mode")
(declare-function iedit-restrict-region "iedit-mode")

(declare-function async-start "async")

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

;;;###autoload
(defun ii/meow--next-change-callback ()
  "If the last mark was saved on different position than current, set mark."
  (let ((last-pos (car mark-ring)))
    (when (not (eq last-pos (point)))
      (push-mark))
    (remove-hook 'post-self-insert-hook 'ii/meow--next-change-callback t)))

(defun ii/meow--search-bounds (regex-p)
  "Return bounds for last regexp search."
  (let* ((rg (car (if regex-p
                      regexp-search-ring
                    search-ring)))
         (forward-fn (if regex-p
                         'search-forward-regexp
                       'search-forward))
         (backward-fn (if regex-p
                          'search-backward-regexp
                        'search-backward))
         (beg (save-mark-and-excursion
                (goto-char (point-min))
                (funcall forward-fn rg (point-max) t 1)
                (goto-char (match-beginning 0))
                (line-beginning-position)))
         (end (save-mark-and-excursion
                (goto-char (point-max))
                (funcall backward-fn rg beg t 1)
                (goto-char (match-end 0))
                (line-end-position))))
    (when (and beg end)
      (cons beg end))))

(defun ii/meow--regexp-search-inner ()
  (ii/meow--search-inner t))

(defun ii/meow--regexp-search-bounds ()
  (ii/meow--search-bounds t))

;;;; Commands

(defun backward-line (&optional n)
  (interactive "p")
  (forward-line (- (or n 1))))

(defun backward-symbol (&optional n)
  (interactive "p")
  (forward-symbol (- (or n 1))))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun ii/meow-command (&optional arg)
  (interactive))

;;;###autoload
(defun ii/meow-mc-mark-all (beg end)
  (interactive (if (region-active-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (let ((search (car regexp-search-ring))
        (case-fold-search nil))
    (if (string= search "")
        (message "Mark aborted")
      (progn
        (mc/remove-fake-cursors)
        (goto-char beg)
        (let ((lastmatch))
          (while (and (< (point) end) ; can happen because of (forward-char)
                      (search-forward-regexp search end t))
            (push-mark (match-beginning 0))
            (mc/create-fake-cursor-at-point)
            (setq lastmatch (point))
            (when (= (point) (match-beginning 0))
              (forward-char)))
          (unless lastmatch
            (error "Search failed for %S" search)))
        (goto-char (match-end 0))
        (if (< (mc/num-cursors) 3)
            (mc/disable-multiple-cursors-mode)
          (mc/pop-state-from-overlay (mc/furthest-cursor-before-point))
          (multiple-cursors-mode 1))))))

;;;###autoload
(defun ii/meow-mc-restrict-to-thing (thing arg)
  "Delete all fake cursors except for ones in bounds of THING. If ARG is a number, use bounds of ARG THINGS. If ARG is negative, use bounds of ARG previous things."
  (interactive (list (meow-thing-prompt "Restrict mc to: ") current-prefix-arg))
  (if-let* ((bounds (meow--parse-bounds-of-thing-char thing))
            (beg (car bounds))
            (end (cdr bounds)))
      (save-mark-and-excursion
        (mc/for-each-fake-cursor
         (when (or (< (overlay-start cursor) beg)
                   (> (overlay-end cursor) end))
           (mc/remove-fake-cursor cursor))))))

;; iedit-mode integration
;;;###autoload
(defun ii/meow-iedit-mode (beg end)
  "Enter `iedit-mode' using the head of `regexp-search-ring' as search regex. BEG and END are bounds of active region or beginning and end of accessible portion of the buffer."
  (interactive (progn
                 ;; if the region is meow word/symbol selection, deactivate it
                 (when (and (region-active-p) meow--search-indicator-overlay)
                   (deactivate-mark))
                 (if (and (region-active-p))
                     (list (region-beginning) (region-end))
                   (list (point-min) (point-max)))))
  (when (region-active-p)
    (deactivate-mark))
  (iedit-start (car regexp-search-ring) beg end)
  (if (> (iedit-counter) 0)
      (iedit-goto-first-occurrence)
    (iedit-mode -1)))

;;;###autoload
(defun ii/meow-iedit-restrict-to-thing (thing arg)
  "Restrict iedit to bounds of THING. If ARG is positive, "
  (interactive (list (meow-thing-prompt "Restrict iedit to: ") current-prefix-arg))
  (when-let* ((bounds (meow--parse-bounds-of-thing-char thing)))
    (iedit-restrict-region (car bounds) (cdr bounds))))

(defun ii/meow-noop ()
  "Does nothing."
  (interactive))

(defun ii/meow-insert-async-shell-command (cmd)
  (interactive (list (read-shell-command "Run: ")))
  (let* ((ov (ii/ui-make-placeholder-overlay (propertize (format "Running %s" cmd) 'face 'error)
                                             cmd
                                             "Waiting"
                                             'success
                                             #'ii/ui-placeholder-animate-elipsis
                                             0.5)))
    (async-start
     (lambda ()
       (shell-command-to-string cmd))
     (lambda (result)
       (with-current-buffer (overlay-buffer ov)
         (save-mark-and-excursion
           (ii/with-no-message!
            (message "result is %S, type is %S" result (type-of result)))
           (goto-char (overlay-start ov))
           (ii/ui-delete-placeholder-overlay ov)
           (ii/ui-make-confirmation-overlay result
                                            (lambda (ov)
                                              (message "Confirmed!")
                                              (save-mark-and-excursion
                                                (let ((content (overlay-get ov 'ii/ui-content)))
                                                  (goto-char (overlay-start ov))
                                                  (ii/ui-delete-confirmation-overlay ov)
                                                  (insert content)))))))))))


(provide 'implicit-meow)
