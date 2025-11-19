;;; -*- lexical-binding: t -*-
;;; Code:
(require 'cl-extra)
(require 'pulse)
(require 'implicit-config-lib)

(defvar-local which-func-line-number nil
  "Line number of current `which-function' entry.")

(eval-when-compile
  (require 'implicit-config-lib))

(declare-function consult-ripgrep "consult")
(declare-function consult--read "consult")
(declare-function -filter "dash")
(declare-function popon-create "popon")
(declare-function doom-color "doom-themes")
(declare-function project-root "project")

(defun ii/make-key-prompt (text key-list)
  "Create a "
  (concat text
          (mapconcat (lambda (cell)
                       (let* ((char (car cell))
                              (help-str (cadr cell)))
                         (concat (propertize (char-to-string char)
                                             'face 'help-key-binding)
                                 " "
                                 help-str)))
                     key-list
                     " | ")))

(defun +lambda-p (form)
  "Return t if FORM is a lambda form, else nil."
  (eq (car form) 'lambda))

(defun any-bound-and-true (symbols)
  (cl-some (lambda (sym)
             (when (bound-and-true-p sym) t))
           symbols))

(defmacro +any-bound-and-true-p! (symbols)
  `(or ,@(mapcar (lambda (sym) `(bound-and-true-p ,sym)) symbols)))

(defun +default-directory ()
  "Get a reasonable guess at a default directory."
  (cond ((bound-and-true-p project-root) (project-root))
        (t default-directory)))

(defun +utils-get-region-contents ()
  "Get the contents in region."
  (when (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

;; (defun +utils-which-func-update-line-number ()
;;   "Update `which-func-line-number' with current number."
;;   (let ((wind (selected-window)))
;;     (setq-local which-func-line-number )))

(defun +utils-whole-buffer-as-string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun +utils--desktop-buffer-predicate (filename bufname mode &rest args)
  (or (when filename
        (string-match-p "\\(gz\\|zip\\|rar\\|tar\\)" (file-name-extension filename)))))

(defun +utils/yank-current-file ()
  "Yank the name of the current file."
  (interactive)
  (let ((name (file-name-nondirectory (buffer-file-name (current-buffer)))))
    (when name
      (message "Yanked %s" name)
      (kill-new name))))

(defun +utils/yank-current-path ()
  "Yank full path of current file."
  (interactive)
  (let ((name (buffer-file-name (current-buffer))))
    (when name
      (message "Yanked %s" name)
      (kill-new name))))

(defun +utils/rename-visited-file ()
  (interactive)
  (rename-visited-file (expand-file-name
                        (read-file-name "Rename visited file to: "
                                        default-directory
                                        nil
                                        nil
                                        (file-name-nondirectory (buffer-file-name))))))

(defun +utils/toggle-mode (mode)
  (interactive)
  (if (and (bound-and-true-p mode))
      (funcall-interactively mode -1)
    (funcall-interactively mode -1)))

(defun +utils/delete-visited-file ()
  "Delete the file visited by current buffer."
  (interactive)
  (let* ((curr-buf (current-buffer))
         (curr-name (buffer-file-name curr-buf)))
    (when (yes-or-no-p (string-join `("Delete " ,curr-name " file?")))
      (kill-buffer curr-buf)
      (delete-file curr-name))))

(defun +utils/copy-visited-file ()
  "Copy the visited file to another location."
  (interactive)
  (let* ((curr-buff (current-buffer))
         (curr-name (buffer-file-name curr-buff)))
    (if (not curr-name)
        (message "This buffer is not visiting any file.")
      (let ((dest (expand-file-name (read-file-name "Destination file: "))))
        (copy-file curr-name dest)
        (find-file dest)))))

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

(defun +utils/forward-defun (arg)
  (interactive "P")
  (beginning-of-defun (if arg
                          (- arg)
                        -1)))

(defun +utils/backward-defun (arg)
  (interactive "P")
  (beginning-of-defun (or arg 1)))

(defun +repeat-negative (arg)
  (interactive "P")
  (when (or (eq last-repeatable-command '+repeat)
            (eq last-repeatable-command '+repeat-negative)
            (eq last-repeatable-command 'repeat))
    (setq last-repeatable-command repeat-previous-repeated-command))
  (setq this-command last-repeatable-command)
  (repeat (if arg (- arg) -1)))

(defun +repeat (arg)
  (interactive "P")
  (when (or (eq last-repeatable-command '+repeat)
            (eq last-repeatable-command '+repeat-negative)
            (eq last-repeatable-command 'repeat))
    (setq last-repeatable-command repeat-previous-repeated-command))
  (setq this-command last-repeatable-command)
  (repeat arg))

(defun +utils/insert-shell-command-output ()
  "Insert output of shell command as string."
  (interactive)
  (let ((f))
    (insert (shell-command-to-string (concat (read-shell-command "Command: ") " &")))))

(defun +utils/insert-shell-command-output-with-preview ()
  "Insert output of shell command as string."
  (interactive)
  (let ((res (shell-command-to-string (concat (read-shell-command "Command: ")))))
    (popon-create res `(1 . 1))))

(defun +utils/browse-modules ()
  "Open user init module file."
  (interactive)
  (let ((default-directory (cdr (project-current nil user-emacs-directory))))
    (project-find-file nil)))

(defun +utils/open-random-file-in-dir (dir)
  (interactive (list default-directory))
  (if-let ((files (seq-remove
                   (lambda (file)
                     (string-equal file (buffer-file-name (current-buffer))))
                   (directory-files dir))))
      (find-alternate-file (seq-random-elt files))
    (error "There are no ther files in %s" dir)))

(defun +utiles/ripgrep-modules ()
  (interactive)
  (consult-ripgrep +init-module-path))

(defun +utils/ripgrep-user-directory ()
  (interactive)
  (consult-ripgrep user-emacs-directory))

(defun ii/negate-repeat-prefix-arg ()
  "Negate "
  (interactive)
  (setq-local last-prefix-arg (- last-prefix-arg)))

(provide 'implicit-utils)
