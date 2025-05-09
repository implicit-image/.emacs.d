;;; -*- lexical-binding: t -*-

(use-package corfu
  :custom-face
  (corfu-border ((t (:background ,(doom-color 'fg-alt)))))
  (corfu-echo ((t (:background ,(doom-color 'bg) :foreground ,(doom-color 'fg)))))
  :custom
  (corfu-cycle t)
  (corfu-doc-delay 0.1)
  (corfu-echo-delay '(0.3 . 0.15))
  (corfu-auto-delay 0.2)
  (corfu-preselect 'prompt)
  (corfu-preview-current 'insert)
  (corfu-auto nil)
  (corfu-popupinfo-delay 0.05)
  (corfu-left-margin-width 3)
  (corfu-right-margin-width 0)
  (corfu-bar-width 0)
  (corfu-count 17)
  (corfu-max-width 50)
  (corfu-quit-no-match t)
  (corfu-on-exact-match 'insert)
  :init
  (defun +corfu--setup ()
    (interactive)
    (completion-preview-mode 1)
    (corfu-echo-mode +1)
    (corfu-history-mode +1)
    (when (not (display-graphic-p))
      (setq-local global-corfu-minibuffer t)
      (corfu-terminal-mode +1))
    (corfu-popupinfo-mode +1))
  :config
  (setf (alist-get 'child-frame-border-width corfu--frame-parameters) 2)
  :hook
  (after-init-hook . global-corfu-mode)
  (nwscript-mode-hook . corfu-mode)
  (corfu-mode-hook . +corfu--setup)
  (company-mode-hook . (lambda ()
                         (interactive)
                         (when (bound-and-true-p corfu-mode)
                           (corfu-mode -1))))
  (lsp-bridge . (lambda ()
                  (corfu-mode -1)))
  :general
  (global-map
   :states 'insert
   "C-TAB" 'completion-at-point
   "C-SPC" 'completion-at-point)
  (corfu-map
   :states 'insert
   "M-h" 'corfu-popupinfo-documentation
   "M-g" 'corfu-info-locationa
   "M-e" 'corfu-expand
   [tab] 'corfu-next
   "<tab>" 'corfu-next
   "TAB" 'corfu-next
   [backtab] 'corfu-previous
   "<backtab>" 'corfu-previous
   "S-TAB" 'corfu-previous))

;; emacs 31 should add tty child frames
(when (< (string-to-number emacs-version) 31)
  ;; for corfu terminal support
  (use-package corfu-terminal
    :custom
    (corfu-terminal-enable-on-minibuffer t)
    (corfu-terminal-disable-on-gui t)
    (corfu-terminal-position-right-margin 5)
    :hook
    (tty-setup-hook . corfu-terminal-mode))

  (use-package corfu-doc-terminal
    :straight (corfu-doc-terminal :type git
                                  :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
    :hook
    (corfu-terminal-mode-hook . corfu-doc-terminal-mode)))

(use-package nerd-icons-corfu
  :functions
  nerd-icons-corfu-formatter
  :custom
  (nerd-icons-corfu-mapping
   '((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
     (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
     (class :style "cod" :icon "symbol_class" :face font-lock-type-face)
     (color :style "fae" :icon "palette_color" :face success)
     (command :style "dev" :icon "terminal_badge" :face default)
     (constant :style "cod" :icon "symbol_constant" :face font-lock-constant-face)
     (constructor :style "cod" :icon "triangle_right" :face font-lock-function-name-face)
     (enummember :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
     (enum-member :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
     (enum :style "cod" :icon "symbol_enum" :face font-lock-builtin-face)
     (event :style "cod" :icon "symbol_event" :face font-lock-warning-face)
     (field :style "cod" :icon "symbol_field" :face font-lock-variable-name-face)
     (file :style "fa" :icon "file_o" :face font-lock-string-face)
     (folder :style "md" :icon "folder" :face font-lock-doc-face)
     (interface :style "cod" :icon "symbol_interface" :face font-lock-type-face)
     (keyword :style "cod":icon "symbol_keyword" :face font-lock-keyword-face)
     (macro :style "md" :icon "exclamation_thick" :face font-lock-keyword-face)
     (magic :style "cod" :icon "wand" :face font-lock-builtin-face)
     (method :style "md" :icon "lambda" :face font-lock-number-face)
     (function :style "md" :icon "lambda" :face font-lock-function-name-face)
     (module :style "oct" :icon "file_submodule" :face font-lock-preprocessor-face)
     (numeric :style "md" :icon "umeric" :face font-lock-builtin-face)
     (operator :style "cod" :icon "symbol_operator" :face font-lock-comment-delimiter-face)
     (param :style "cod" :icon "symbol_parameter" :face default)
     (property :style "cod" :icon "symbol_property" :face font-lock-variable-name-face)
     (reference :style "cod" :icon "references" :face font-lock-variable-name-face)
     (snippet :style "fa" :icon "code" :face font-lock-string-face)
     (string :style "oct" :icon "quote" :face font-lock-string-face)
     (struct :style "md" :icon "code_braces" :face font-lock-variable-name-face)
     (text :style "cod" :icon "text_size" :face font-lock-doc-face)
     (typeparameter :style "oct" :icon "list_unordered" :face font-lock-type-face)
     (type-parameter :style "oct" :icon "list_unordered" :face font-lock-type-face)
     (unit :style "cod" :icon "symbol_ruler" :face font-lock-constant-face)
     (value :style "cod" :icon "symbol_field" :face font-lock-builtin-face)
     (variable :style "md" :icon "variable" :face font-lock-variable-name-face)
     (t :style "oct" :icon "code_square" :face font-lock-warning-face)))
  :init
  (with-eval-after-load 'corfu
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

(provide 'init-corfu)
