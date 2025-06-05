;;; -*- lexical-binding: t -*-
(require 'cl-extra)

(defun any-bound-and-true (symbols)
  (cl-some (lambda (sym)
             (when (bound-and-true-p sym) t))
           symbols))

(defun +default-directory ()
  "Get a reasonable guess at a default directory."
  (cond ((bound-and-true-p projectile-project-root) (projectile-project-root))
        ((bound-and-true-p project-root) (project-root))
        (t default-directory)))

(defun +get-region-contents ()
  "Get the contents in region."
  (when (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun +utils-whole-buffer-as-string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

;;;###autoload
(defun +utils/yank-current-file ()
  "Yank the name of the current file."
  (interactive)
  (let ((name (file-name-nondirectory (buffer-file-name (current-buffer)))))
    (when name
      (message "Yanked %s" name)
      (kill-new name))))

;;;###autoload
(defun +utils/yank-current-path ()
  "Yank full path of current file."
  (interactive)
  (let ((name (buffer-file-name (current-buffer))))
    (when name
      (message "Yanked %s" name)
      (kill-new name))))

;;;###autoload
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

;;;###autoload
(defun +utils/insert-shell-command-output ()
  "Insert output of shell command as string."
  (interactive)
  (insert (shell-command-to-string (concat (read-shell-command "Command: ") " &"))))

;;;###autoload
(defun +utils/insert-shell-command-output-with-preview ()
  "Insert output of shell command as string."
  (interactive)
  (let ((res (shell-command-to-string (concat (read-shell-command "Command: ")))))
    (popon-create res `(1 . 1))))

;;;###autoload
(defun +utils/browse-modules ()
  "Open user init module file."
  (interactive)
  (consult--read (mapcar (lambda (path)
                           (string-trim-left path (format "^%s/" +init-module-path)))
                         (-filter (lambda (path)
                                    (string-suffix-p ".el" path))
                                  (directory-files-recursively +init-module-path ".*\\.el")))
                 :prompt "Open init module file: "
                 :require-match t
                 :lookup (lambda (name &rest args)
                           ;; (message (format "entry is %s" (file-name-concat +init-module-path name)))
                           (find-file (file-name-concat +init-module-path name)))))

;;;###autoload
(defun +utiles/ripgrep-modules ()
  (interactive)
  (consult-ripgrep +init-module-path))

;;;###autoload
(defun +utils/ripgrep-user-directory ()
  (interactive)
  (consult-ripgrep user-emacs-directory))

(provide 'init-utils)
