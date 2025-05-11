;;; -*- lexical-binding: t -*-


(defvar +indent/tab-jump-delims '(?\; ?\) ?\( ?\] ?\[ ?{ ?} ?> ?< ?| ?' ?` ?\. ?\"))

(defvar-local +indent-tab-function nil)

(defmacro +contrast-color! (color)
  "Return a color contrasting well with COLOR."
  `(apply
    ,(if (< (color-distance color "#000000") 180000)
         'doom-lighten
       'doom-darken)
    color
    0.3
    nil))

(defmacro +setq! (&rest symbols)
  "Set SYMBOLS to associated values while taking care of setting default options."
  `(dolist (binding ,( symbols))))


(defun +toggle-var (var)
  (set var (not (symbol-value var))))

(defun +utils/toggle-mode (mode)
  (interactive)
  (if (and (bound-and-true-p mode))
      (funcall-interactively mode -1)
    (funcall-interactively mode -1)))

(defmacro +nth (n list)
  "Returnd nth element of `list'. If `n' is greater than length of `list' takes `(mod n (length list))' instead."
  (nth (mod (length list) n) list))

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

(defun +utils-whole-buffer-as-string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun +utils/insert-shell-command-output ()
  "Insert output of shell command as string."
  (interactive)
  (insert (shell-command-to-string (concat (read-shell-command "Command: ") " &"))))

(defun +utils/insert-shell-command-output-with-preview ()
  "Insert output of shell command as string."
  (interactive)
  (let ((res (shell-command-to-string (concat (read-shell-command "Command: ")))))
    (popon-create res `(1 . 1))))

(defun +default-directory ()
  "Get a reasonable guess at a default directory."
  (cond ((bound-and-true-p projectile-project-root) (projectile-project-root))
        ((bound-and-true-p project-root) (project-root))
        (t default-directory)))

(defun +char-whitespace? (char)
  (memq char '(32 160 9)))

(defun +get-region-contents ()
  "Get the contents in region."
  (interactive)
  (when (use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defmacro +set-tab-function! (mode function &optional hook)
  "Set default tab function FUNCTION for MODE in HOOK."
  (let ((fun (intern (concat "+" (symbol-name mode) "-indent-setup")))
        (hook (or (when (bound-and-true-p hook) hook)
                  (intern (concat (symbol-name mode) "-hook")))))
    `(progn (defun ,fun ()
              (setq-local +indent-tab-function ',function))
            (add-hook (quote ,hook) (quote ,fun)))))

(defun +smart-tab (&optional prefix)
  ""
  (interactive "P")
  (let ((next (char-after (point))))
    (cond ((bound-and-true-p +indent-tab-function) (funcall-interactively +indent-tab-function))
          ((memq next +indent/tab-jump-delims) (forward-char))
          ((+char-whitespace? next) (forward-whitespace 1))
          ((eolp) (yasnippet-capf))
          (t (indent-for-tab-command prefix)))))

(use-package simple
  :straight nil
  :init
  (setq backward-delete-char-untabify-method 'hungry)
  :hook
  ((help-mode-hook helpful-mode-hook lsp-ui-doc-mode-hook) . visual-line-mode))

;; (+leader-keys
;;   "f D" '("Delete current file" . +utils/delete-visited-file)
;;   "f C" '("Copy current file" . +utils/copy-visited-file)
;;   "q A" '("Save all and kill emacs" . save-buffers-kill-emacs)
;;   "t c" '("Colorize color strings" . rainbow-mode)
;;   "t I" '("Select input method" . set-input-method)
;;   "t v" '("Visual line mode" . visual-line-mode)
;;   "i !" '("Shell command" . +utils/insert-shell-command-output))

(provide 'init-utils)
