;;; -*-lexical-binding: t-*-

(defun +meow-eval-command (&optional insert)
  (minibuffer-with-setup-hook
      (lambda ()
        (set-syntax-table emacs-lisp-mode-syntax-table)
        (add-hook 'completion-at-point-functions
                  #'elisp-completion-at-point nil t)
        (setq-local trusted-content :all)
        (run-hooks 'eval-expression-minibuffer-setup-hook))
    (let ((command (read-from-minibuffer prompt initial-contents
                                         read--expression-map t
                                         'read-expression-history)))
      (message command))))

(use-package meow
  :custom
  (meow-use-keypad-when-execute-kbd nil)
  :init

  (defun +meow-minibuffer-command (&optional prefix)
    ""
    (interactive)
    )

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-define-key
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
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet)
     ;; applications
     '("a" . "C-x SPC a")
     ;; file commands
     '("f" . "C-x SPC f")
     ;; code commands
     '("c" . "C-x SPC c")
     ;; buffer commands
     '("b" . "C-x SPC b")
     ;; help commands
     '("h" . "C-x SPC h")
     ;; projectile commands
     '("p" . "C-c p")
     ;; combobulate commands)
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
      '(";" . meow-reverse)
      '("," . meow-inner-of-thing)
      '("." . meow-bounds-of-thing)
      '("[" . meow-beginning-of-thing)
      '("]" . meow-end-of-thing)
      '("a" . meow-append)
      '("A" . meow-open-below)
      '("b" . meow-back-word)
      '("B" . meow-back-symbol)
      '("c" . meow-change)
      '("d" . meow-delete)
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
      '("q" . meow-quit)
      '("Q" . meow-goto-line)
      '("r" . meow-replace)
      '("R" . meow-swap-grab)
      '("s" . meow-kill)
      '("t" . meow-till)
      '("u" . meow-undo)
      '("U" . meow-undo-in-selection)
      '("v" . meow-visit)
      '("w" . meow-mark-word)
      '("W" . meow-mark-symbol)
      '("x" . meow-line)
      '("x" . meow-goto-line)
      '("y" . meow-save)
      '("y" . meow-sync-grab)
      '("z" . meow-pop-selection)
      '("'" . repeat)
      '("<escape>" . ignore))


     ;; setup custom meow keymaps
     (setq meow-window-keymap (make-keymap)
           meow-lsp-keymap (make-keymap)
           meow-combobulatep-keymap (make-keymap))

     (meow-define-state window
       "meow state for window navigation"
       :keymap meow-window-keymap)

     (meow-define-state lsp-commands
       "meow state for lsp commands."
       :keymap meow-lsp-keymap)

     (meow-define-state combobulate
       "meow state for combobulate"
       :keymap meow-combobulate-keymap))

    (defun +load-meow ()
      (require 'meow)
      (meow-setup)
      (meow-global-mode 1))

    :hook
    (after-init-hook . +load-meow))

  (provide 'init-meow)
