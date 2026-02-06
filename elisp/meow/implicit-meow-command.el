;;; -*- lexical-binding: t -*-
(require 'meow)

(defvar ii/meow-command-history nil
  "History for `+meow/command'.")

(defvar ii/meow-command-prompt (propertize ":" 'read-only t 'cursor-intangible t 'intangible t 'insert-behind-hooks '(ii/meow-command--insert-update)))

(defvar ii/meow-command--short-form-alist nil)

(defvar ii/meow-command--long-form-alist nil)

(defvar ii/meow-command-alist nil)


;;;; Macros

(defmacro ii/meow-define-command (name &rest body)
  "Define a meow command with NAME and BODY."
  (declare (indent defun))
  (if-let* ((short-name (plist-get body :short))
            (args (plist-get body :args))
            (fn (plist-get body :function)))
      `(progn (add-to-list 'ii/meow-command-alist (list ,name ,fn))
              (add-to-list 'ii/meow-command--short-form-alist (list ,short-name ,fn)))))

(defmacro ii/meow-define-command-prefix (name char)
  "Define `meow' command prefix NAME as CHAR."
  `(progn ))



(defun ii/meow-command--parse (command-expr)
  "Parse `meow' commands in COMMAND-EXPR."
  (if-let* ((command (car (string-split command-expr " ")))
            (fn (assq (intern ))))
      ()
    (progn (message "Command not found")
           nil)))

(defun ii/meow-command--capf ()
  "`completion-at-point' function for meow commands.")

(defun ii/meow-command--insert-update (beg end)
  "")

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

(defun ii/meow-command--minibuffer-setup ()
  (set-syntax-table emacs-lisp-mode-syntax-table)
  (cursor-intangible-mode 1)
  (add-hook 'completion-at-point-functions
            #'cape-elisp-symbol nil t)
  (add-hook ')
  (setq-local trusted-content :all))

(defun +meow/command (&optional arg)
  "Prompt for a command to execute."
  (interactive "p")
  (minibuffer-with-setup-hook 'ii/meow-command--minibuffer-setup
    (let* ((command (read-from-minibuffer ii/meow-command-prompt
                                          nil
                                          read--expression-map
                                          nil
                                          'ii/meow-command-history)))
      (pcase command
        ((guard (or (string-equal (string-clean-whitespace command)
                                  "0")
                    (not (eq (string-to-number command) 0))))
         (goto-line (string-to-number command)))
        ("w" (progn (save-buffer)))
        ("wa" (save-some-buffers))
        ("wq" (progn (save-buffer)
                     (kill-buffer-and-window)))
        ((or "wqa" "qwa")
         (save-buffers-kill-emacs))
        ("q" (kill-buffer-and-window))
        ("qa" (kill-emacs))
        ((guard (readablep command))
         (cond ((commandp (read command)) (command-execute (read command)))
               ((and (symbolp (read command))
                     (not (boundp (read command))))
                (+meow--error "[meow] command %s not found" command))
               (t (+meow--info "%S" (eval (read command))))))
        (_ (+meow--error "[meow] command error"))))))


;;;; commands

(ii/meow-define-command write
  :short "w"
  :args (())
  :function
  (lambda (path)
    (write-file )))

(provide 'implicit-meow-command)
