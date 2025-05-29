;;; -*-lexical-binding: t-*-

(defun +meow-command (&optional insert)
  (interactive)
  (minibuffer-with-setup-hook
      (lambda ()
        (set-syntax-table emacs-lisp-mode-syntax-table)
        (add-hook 'completion-at-point-functions
                  #'elisp-completion-at-point nil t)
        (setq-local trusted-content :all))
    ;; (run-hooks 'eval-expression-minibuffer-setup-hook))
    (let ((command (read-from-minibuffer ":"
                                         nil
                                         read--expression-map
                                         nil
                                         'read-expression-history)))
      (cond ((string-match-p "[wqa!]+.*" command)
             (let ((write (s-contains-p "w" command))
                   (quit (s-contains-p "q" command))
                   (all (s-contains-p "a" command))
                   (force (s-contains-p "!" command)))
               (if write
                   (if all
                       (save-some-buffers)
                     (save-buffer)))))
            (t (let ((expr (read command)))
                 (cond ((commandp expr) (funcall expr))
                       (t (eval-expression expr)))))))))

(defun +meow-mark-word (&optional n)
  "Make meow-mark-word act like helix."
  (interactive "p")
  (when (region-active-p)
    (deactivate-mark)
    (forward-word n))
  (meow-mark-word n))

(defun +meow-mark-symbol (&optional n)
  (interactive "p")
  (when (region-active-p)
    (deactivate-mark)
    (forward-symbol n))
  (meow-mark-symbol n))

(defun +meow-back-word (&optional n)
  (interactive "p")
  (when (region-active-p)
    (deactivate-mark)
    (backward-word n))
  (meow-back-word n))

(defun +meow-back-symbol (&optional n)
  (interactive "p")
  (when (region-active-p)
    (deactivate-mark)
    (backward-symbol n))
  (meow-back-symbol n))

(defun +meow-search (&optional regexp)
  (interactive "p")
  (if (region-active-p)
      (progn (meow-grab)
             (isearch-forward regexp))
    (isearch-forward regexp)))

(use-package meow-tree-sitter
  :config
  (meow-tree-sitter-register-defaults))

(use-package meow
  :custom
  (meow-use-keypad-when-execute-kbd nil)
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-define-key
     '("q" . meow-quit)
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("?" . meow-keypad-describe-key))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(":" . +meow-command)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("}" . "M-}")
     '("{" . "M-{")
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . +meow-back-word)
     '("B" . +meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-kill)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . quoted-insert)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . +meow-select)
     '("t" . meow-till)
     '("u" . +meow-undo)
     '("U" . undo-redo)
     '("v" . meow-visit)
     '("w" . +meow-mark-word)
     '("W" . +meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)
     '("/" . isearch-forward)
     '("?" . isearch-backward))

    ;; setup custom meow keymaps
    (setq meow-combobulate-keymap (make-keymap))

    (setq meow-keypad-leader-dispatch "C-x SPC")

    (meow-define-state combobulate
      "meow state for combobulate"
      :keymap meow-combobulate-keymap))

  ;; kbd macro overrides
  (setq meow--kbd-kill-region "S-<delete>")

  (setf (alist-get 'meow-kill meow-selection-command-fallback)
        'meow-delete)

  (defun +load-meow ()
    (require 'meow)
    (meow-setup)
    (meow-global-mode 1))
  :hook
  (after-init-hook . +load-meow))

(provide 'init-meow)
