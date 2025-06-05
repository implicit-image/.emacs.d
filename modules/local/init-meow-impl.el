(require 'meow)

(eval-when-compile
  (require 'mc-mark-more))

(defvar meow-command-history nil
  "History for `+meow-command'.")

(defun +meow-command-write-p (expr)
  (or (string-match-p "w .+" expr)))

(defun +meow-command--get-command (expr)
  (cond ((or (eq expr "w")
             (eq expr "write")
             (string-match-p "w[qa!]*" expr)
             'write)
         ((or (eq expr "q")
              (eq expr "quit")
              (string-match-p "q[wa!]*" expr))
          'quit)
         (t nil))))

(defun +meow-command--get-arguments (expr args)
  (pcase cmd
    ('write)))

(defun +meow--select-for-mc (&optional regexp)
  (interactive "p")
  (apply (if regexp
             'mc/mark-all-in-region
           'mc/mark-all-in-region-regexp)
         (list (region-beginning) (region-end))))

(defun +meow--surround-get-matching (char)
  (let* ((open (pcase char
                 (?\) ?\()
                 (?\] ?\[)
                 (?\} ?\{)
                 (?\> ?\<)
                 (_ char)))
         (close (pcase char
                  (?\( ?\))
                  (?\[ ?\])
                  (?\{ ?\})
                  (?\< ?\>)
                  (_ open))))
    (cons open close)))

(defun +meow--surround-thing-with (arg open close)
  (save-mark-and-excursion
    (insert-pair nil open close))
  (when arg
    (save-mark-and-excursion
      (let* ((beg (region-beginning))
             (end (region-end))
             (char-start (1+ beg))
             (char-end (1+ end)))
        ;; point is always BEFORE the character
        (goto-char char-start)
        ;; reset point position to the start of the expression
        (set-mark (point))
        (insert ?\n)
        (goto-char char-end)
        (insert ?\n)
        (forward-char)
        (indent-region (region-beginning)
                       (region-end))))))

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

;;;###autoload
(defun backward-line (&optional n)
  (interactive "p")
  (forward-line (- (or n 1))))

;;;###autoload
(defun backward-symbol (&optional n)
  (interactive "p")
  (forward-symbol (- (or n 1))))

;;;###autoload
(defun +meow/command (&optional arg)
  (interactive "p")
  (minibuffer-with-setup-hook
      (lambda ()
        (set-syntax-table emacs-lisp-mode-syntax-table)
        (add-hook 'completion-at-point-functions
                  #'elisp-completion-at-point nil t)
        (setq-local trusted-content :all))
    ;; (run-hooks 'eval-expression-minibuffer-setup-hook))
    (let* ((command (read-from-minibuffer ":"
                                          nil
                                          read--expression-map
                                          nil
                                          'meow-command-history))
           (result (pcase command
                     ("w" (progn (save-buffer)))
                     ("wa" (save-some-buffers))
                     ("wq" (progn (save-buffer)
                                  (kill-buffer-and-window)))
                     ((or "wqa"
                          "qwa")
                      (save-buffers-kill-emacs))
                     ("q" (kill-buffer-and-window))
                     ("qa" (kill-emacs))
                     ((guard (readablep command)) (cond ((commandp (read command)) (command-execute (read command)))
                                                        ((and (symbolp (read command))
                                                              (not (boundp (read command))))
                                                         (+meow--error "[meow] command %s not found" command))
                                                        (t (eval (read command)))))
                     (_ (+meow--error "[meow] command error"))))))))

;;;###autoload
(defun +meow/select-for-mc-string ()
  (interactive)
  (funcall-interactively '+meow--select-for-mc nil))

;;;###autoload
(defun +meow/select-for-mc-regex ()
  (interactive)
  (funcall-interactively '+meow--select-for-mc t))

;;;###autoload
(defun +meow/surround-thing (&optional with-newline)
  "Surround a thing with provided character.  If prefix argument WITH-NEWLINE is non-nil,\
 insert newline after the opening character and before the closing one, \
indenting the region afterwards."
  (interactive "P")
  (when (not (region-active-p))
    (meow-bounds-of-thing (meow-thing-prompt "Surround: ")))
  (let* ((chars (+meow--surround-get-matching (read-char "Surround with: ")))
         (open (car chars))
         (close (cdr chars)))
    (if (and open close)
        (+meow--surround-thing-with with-newline
                                    open
                                    close)
      (message "Failed determining matching characters"))))

(provide 'init-meow-impl)
