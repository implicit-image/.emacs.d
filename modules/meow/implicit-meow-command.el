;;; -*- lexical-binding: t -*-
(require 'meow)

(defvar meow-command-history nil
  "History for `+meow/command'.")

(defvar ii/meow-command-prompt (propertize ":" 'cursor-intangible t))

(defvar ii/meow-command--short-form-alist nil)

(defvar ii/meow-command--long-form-alist nil)

(defmacro ii/meow-define-command (name &rest body)
  "Define a meow command with NAME and BODY."
  (declare (indent defun))
  (let ((fname (intern (concat "ii/meow--command-" (symbol-name name))))
        (short-form (eval `(concat ,@(map (lambda (str)
                                            (string-to-char str))
                                          (string-split (symbol-name name)
                                                        "-" t t)) ))))
    (add-to-list 'ii/meow-command--short-form-alist `(,short-form . ,fname))
    (add-to-list 'ii/meow-command--long-form-alist `(,name . ,fname))
    `(defun  ,fname (&optional prefix-arg)
       (interactive "P")
       ,@body)))

(defun ii/meow-command--capf ()
  "Capf function for meow commands.")


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

(defun +meow/command (&optional arg)
  "Prompt for a command to execute."
  (interactive "p")
  (minibuffer-with-setup-hook
      (lambda ()
        (set-syntax-table emacs-lisp-mode-syntax-table)
        (add-hook 'completion-at-point-functions
                  #'cape-elisp-symbol nil t)
        (setq-local trusted-content :all))
    (let* ((command (read-from-minibuffer ii/meow-command-prompt
                                          nil
                                          read--expression-map
                                          nil
                                          'meow-command-history)))
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



(provide 'implicit-meow-command)
