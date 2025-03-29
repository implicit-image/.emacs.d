;;; -*- lexical-binding: t -*-


(defmacro +setq! (&rest symbols)
  "Set SYMBOLS to associated values while taking care of setting default options."
  `(dolist (binding ,( symbols))))

(defun +toggle-var (var)
  (set var (not (symbol-value var))))

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




(use-package simple
  :straight nil
  :hook
  ((help-mode helpful-mode lsp-ui-doc-mode) . visual-line-mode))


(+leader-keys
  "f D" '("Delete current file" . +utils/delete-visited-file)
  "f C" '("Copy current file" . +utils/copy-visited-file)
  "q A" '("Save all and kill emacs" . save-buffers-kill-emacs)
  "t c" '("Colorize color strings" . rainbow-mode)
  "t I" '("Select input method" . set-input-method)
  "t v" '("Visual line mode" . visual-line-mode)
  "i !" '("Shell command" . +utils/insert-shell-command-output))

(provide 'init-utils)
