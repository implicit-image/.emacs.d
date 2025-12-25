;;; -*- lexical-binding: t -*-
;; set up straight.el

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(when (< emacs-major-version 31)
  (straight-use-package 'use-package))

;; make use-package use straight.el by default
(setq straight-use-package-by-default t
      use-package-hook-name-suffix nil
      ;; lazy load by default
      use-package-always-defer t
      use-package-ignore-unknown-keywords (not init-file-debug)
      use-package-compute-statistics init-file-debug
      use-package-expand-minimally (not init-file-debug))

;; early org mode declaration to make sure the correct version gets loaded
(use-package org
  :straight `(org
              :fork (:host nil
                           :repo "https://git.tecosaur.net/tec/org-mode.git"
                           :branch "dev"
                           :remote "tecosaur")
              :files (:defaults "etc")
              :build t
              :pre-build
              (with-temp-file "org-version.el"
                (require 'lisp-mnt)
                (let ((version
                       (with-temp-buffer
                         (insert-file-contents "lisp/org.el")
                         (lm-header "version")))
                      (git-version
                       (string-trim
                        (with-temp-buffer
                          (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                          (buffer-string)))))
                  (insert
                   (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
                   (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
                   "(provide 'org-version)\n")))
              :pin nil))

(defvar +init-module-path (expand-file-name "modules" user-emacs-directory))

(defvar ii/elisp-path (expand-file-name "elisp" user-emacs-directory))

(defvar +init-autoload-path (expand-file-name "local" +init-module-path))

(add-to-list 'load-path +init-module-path)

(defvar +per-machine-config-feature (intern (concat "implicit-" (system-name))))

;; add everything to `load-path'
(dolist (path (directory-files (expand-file-name "modules" user-emacs-directory)
                               t
                               "[^\.]+"))
  (add-to-list 'load-path path))

(require 'implicit-config-lib)

(+set-exec-path-from-shell)
(+set-env-vars-from-shell "JAVA_HOME" "TERM" "EDITOR")

;;;; Elisp Libraries
(use-package async
  :init
  (setq dired-async-skip-fast t
        dired-async-small-file-max 5000000)
  (with-eval-after-load 'dired
    (dired-async-mode 1)))

(defun ii/hide-modeline ()
  (if mode-line-format
      (setq-local mode-line-format nil)))

;;;; base Config
(setq meow-leader-global-map (make-sparse-keymap)
      meow-eat-state-map (make-sparse-keymap)
      meow-vterm-state-map (make-sparse-keymap)
      meow-eat-toggle-map (make-sparse-keymap)
      meow-mc-global-map (make-sparse-keymap)
      flymake-prefix-map (make-sparse-keymap)
      meow-error-repeat-map (make-sparse-keymap)
      next-defun-repeat-map (make-sparse-keymap)
      outline-repeat-map (make-sparse-keymap)
      transpose-repeat-map (make-sparse-keymap))

(define-prefix-command 'meow-toggle-prefix-command 'meow-toggle-prefix-map "toggle")
(define-prefix-command 'meow-quit-prefix-command 'meow-quit-prefix-map "quit")
(define-prefix-command 'meow-vc-prefix-command 'meow-vc-prefix-map "vc")
(define-prefix-command 'meow-window-prefix-command 'meow-window-prefix-map "window")
(define-prefix-command 'meow-code-prefix-command 'meow-code-prefix-map "code")
(define-prefix-command 'meow-music-prefix-command 'meow-music-prefix-map "music")
(bind-key "C-c t" meow-toggle-prefix-map)
(bind-key "C-c q" meow-quit-prefix-map)
(bind-key "C-c g" meow-vc-prefix-map)
(bind-key "C-w" 'meow-window-prefix-command)
(bind-key "C-c c" meow-code-prefix-map)
(bind-key "C-c m" meow-music-prefix-map)
(setopt indent-tabs-mode nil
        blink-cursor-mode nil)

(setq-default backup-inhibited t
              create-lockfiles nil
              truncate-lines t
              truncate-partial-width-windows nil
              indicate-buffer-boundaries nil
              cursor-in-non-selected-windows nil
              indicate-empty-lines nil
              tab-width 4)

(setq user-full-name "Błażej Niewiadomski"
      user-mail-address "blaz.nie@protonmail.com"
      visible-bell nil
      display-line-numbers-type 'relative
      hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil
      global-hl-line-buffers '(not
                               (or (lambda (b) (buffer-local-value 'cursor-face-highlight-mode b))
                                   (lambda (b) (string-match-p "\\` " (buffer-name b))) minibufferp
                                   (major-mode . eat-mode)
                                   (major-mode . dired-sidebar-mode)))
      truncate-partial-width-windows nil
      x-stretch-cursor nil
      make-backup-files nil
      backup-by-copying t
      echo-keystrokes 0.01
      scroll-step 0
      scroll-conservatively 100
      scroll-preserve-screen-position nil
      scroll-margin 7
      scroll-error-top-bottom t
      comment-multi-line t
      comment-empty-lines t
      lazy-highlight-initial-delay 0
      completion-ignore-case t
      sentence-end-double-space nil
      read-extended-command-predicate 'command-completion-default-include-p
      use-dialog-box nil
      use-file-dialog nil
      use-short-answers t
      read-answer-short t
      delete-selection-save-to-register t
      register-use-preview t
      next-screen-context-lines 0
      warning-minimum-level :error
      auto-window-vscroll nil
      save-place-file (expand-file-name "saveplace" user-emacs-directory)
      save-place-limit 600
      tab-always-indent 'complete
      tab-first-completion 'word
      desktop-dirname user-emacs-directory
      desktop-restore-frames nil
      desktop-restore-reuse-frames nil
      desktop-modes-not-to-save '(fundamental-mode tags-table-mode image-mode pdf-view-mode nov-mode org-mode)
      large-file-warning-threshold (* 30 1000 1000)
      duplicate-line-final-position -1
      duplicate-region-final-position -1
      proced-enable-color-flag t
      proced-auto-update-flag 'visible
      visible-cursor nil
      global-mark-ring-max 32
      mark-ring-size 32
      speedbar-use-images nil
      speedbar-prefer-window t
      speedbar-window-default-width 40
      speedbar-window-max-width 40
      savehist-additional-variables
      '(kill-ring
        register-alist
        mark-ring
        global-mark-ring
        search-ring
        regexp-search-ring
        kmacro-ring)
      kmacro-ring-max 32
      kmacro-counter-value-start 1
      imenu-auto-rescan t
      imenu-use-popup-menu 'on-mouse
      text-scale-mode-step 1.1)

(bind-keys* ("C-=" . text-scale-increase)
            ("C--" . text-scale-decrease))

(let ((customization-file (expand-file-name "custom.el" user-emacs-directory)))
  (unless (file-exists-p customization-file)
    (write-region "" nil customization-file))
  (setq custom-file customization-file)
  (load custom-file 'noerror))


(electric-pair-mode 1)
(global-hl-line-mode 1)
(repeat-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(column-number-mode 1)
(delete-selection-mode 1)
;; (desktop-save-mode 1)
(set-frame-font +base/font-spec nil t t)
(set-face-attribute 'default t
                    :font +base/font-spec)
;; add an option to diff current buffer with its file on disk
(add-to-list 'save-some-buffers-action-alist
             (list "d"
                   (lambda (buffer)
                     (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))

(use-package emacs
  :config
  (require 'server)
  (if (not (server-running-p))
      (server-start))
  :bind-keymap*
  ("`" . esc-map)
  :bind*
  (("M-RET" . recenter)
   ("M-<return>" . recenter)
   ("M-]" . forward-paragraph)
   ("M-[" . backward-paragraph)
   ("M-g g" . beginning-of-buffer)
   ("M-g e" . end-of-buffer)
   ("M-g f" . find-file-at-point)
   ("M-g w" . browse-url-at-point)
   ("M-g j" . next-line)
   ("M-g k" . previous-line)
   ("M-g TAB" . nil)
   ("M-g |" . move-to-column)
   ("M-g l" . move-end-of-line)
   ("M-g h" . move-beginning-of-line)
   ("M-g s" . forward-whitespace)
   ("M-g +" . duplicate-dwim)
   ("C-c to" . toggle-option)
   ("C-c tde" . toggle-debug-on-error)
   ("C-c tdq" . toggle-debug-on-quit)
   ("C-c tl" . scroll-lock-mode)
   ("C-c qA" . save-buffers-kill-emacs)
   ("C-c qa" . kill-emacs)
   ("C-c qb" . kill-current-buffer)
   ("C-c qr" . restart-emacs)
   ("C-c ]p" . forward-paragraph)
   ("C-c [p" . backward-paragraph)
   ("C-c [f" . beginning-of-defun)
   ("C-c ]s" . forward-sexp)
   ("C-c [s" . backward-sexp)
   ("C-c ]]" . forward-list)
   ("C-c [[" . backward-list)
   ("C-c ]}" . up-list)
   ("C-c [}" . backward-up-list)
   ("C-c ]i" . forward-to-indentation)
   ("C-c [i" . backward-to-indentation)
   ("C-c ]l" . forward-line)
   ("C-c [l" . backward-line)
   ("C-x K" . kill-buffer)
   ("C-c bk" . kill-buffer))
  :hook
  ;; better to see all of the code
  ((window-setup-hook server-after-make-frame-hook) . +font--setup)
  ((help-mode-hook helpful-mode-hook lsp-ui-doc-hook) . visual-line-mode)
  ;; display line numbers in text-editing modes
  (prog-mode-hook . visual-line-mode)
  (visual-line-mode-hook . visual-wrap-prefix-mode)
  (gfm-mode-hook . display-line-numbers-mode)
  (prog-mode-hook . display-line-numbers-mode)
  (after-init-hook . (lambda () (message (emacs-init-time)))))

(use-package implicit-utils
  :straight `(implicit-utils :type nil
                             :local-repo ,(expand-file-name "config" +init-module-path))
  :autoload
  (+utils-whole-buffer-as-string
   +utils-get-region-contents
   +utils--desktop-buffer-predicate)
  :commands
  (+utils/open-random-file-in-dir)
  :bind*
  (("M-<backspace>" . backward-kill-word)
   ("C-c f%" . +utils/open-random-file-in-dir)
   ("C-c fC" . +utils/copy-visited-file)
   ("C-c fD" . +utils/delete-visited-file)
   ("C-c fP" . +utils/browse-modules)
   ("C-c fp" . +utils/ripgrep-user-directory)
   ("C-c fR" . +utils/rename-visited-file)
   ("C-c fy" . +utils/yank-current-file)
   ("C-c fY" . +utils/yank-current-path)
   ("C-c tf" . +utils/consult-set-font-family)
   ("C-c i!" . +utils/insert-shell-command-output)
   ("C-c ]f" . +utils/forward-defun)
   :repeat-map next-defun-repeat-map
   ("\]" . +utils/forward-defun)
   ("\[" . +utils/backward-defun)))

;;(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?┃))
(set-display-table-slot standard-display-table 'truncation (make-glyph-code 32))

(setq which-key-popup-type 'side-window
      which-key-preserve-window-configuration nil
      which-key-side-window-max-width 0.2
      which-key-idle-delay 0.6
      which-key-side-window-max-height 0.2
      which-key-idle-secondary-delay 0.05
      which-key-separator " "
      which-key-sort-order 'which-key-key-order-alpha
      which-key-side-window-slot 2
      which-key-max-display-columns 5
      which-key-prefix-prefix "[M] "
      which-key-add-column-padding 2
      which-key-show-remaining-keys nil
      which-key-frame-max-width 2
      which-key-frame-max-height 10
      which-key-min-column-description-width 30)

(add-hook 'meow-global-mode-hook
          (defun ii/which-key--setup ()
            (require 'which-key)
            (which-key-mode)
            (which-key-setup-side-window-bottom)))

(use-package kkp
  :hook
  (tty-setup-hook . global-kkp-mode))

(use-package term-keys
  :straight (term-keys :type git
                       :host github
                       :repo "CyberShadow/term-keys")
  :hook
  (tty-setup-hook . term-keys-mode))

(use-package xclip
  :if (+os/is-linux-p)
  :hook
  (tty-setup-hook . xclip-mode))

(use-package implicit-meow
  :straight `(implicit-meow :type nil
                            :local-repo ,(expand-file-name "meow" +init-module-path))
  :after meow
  :commands
  (+meow/command
   +meow/yank
   ii/meow--next-change-callback
   ii/meow-switch-char-case
   ii/meow-switch-case
   ii/meow-toggle-case-region
   ii/meow-upcase-dwim
   ii/meow-downcase-dwim
   ii/meow-change-number-at-point
   ii/meow-increment-number-at-point
   ii/meow-decrement-number-at-point
   ii/meow--setup-local-pairs)
  :autoload
  (ii/meow--beacon-mode-setup)
  :preface
  (setq ii/meow-toggle-case-repeat-map (make-sparse-keymap))
  :config
  (add-to-list 'meow-selection-command-fallback '(ii/meow-toggle-case . ii/meow-toggle-char-case))
  :bind*
  (("M-g C-c" . ii/meow-switch-case)
   ("M-g ~" . ii/meow-toggle-case-region)
   ("M-g u" . ii/meow-downcase-dwim)
   ("M-g U" . ii/meow-upcase-dwim)
   ("C-c ei" . ii/meow-increment-number-at-point)
   ("C-c ed" . ii/meow-decrement-number-at-point)
   :repeat-map ii/meow-toggle-case-repeat-map
   ("~" . ii/meow-toggle-case-region)
   ("C-c" . ii/meow-switch-case)
   ("~" . ii/meow-toggle-case-region)
   ("u" . ii/meow-downcase-dwim)
   ("U" . ii/meow-upcase-dwim))
  :hook
  (meow-beacon-mode-hook . ii/meow--beacon-mode-setup)
  (after-change-major-mode-hook . ii/meow--setup-local-pairs))

(use-package meow-tree-sitter
  :init

  (defun ii/meow-tree-sitter-register-thing (key types &optional query)
    "Register `meow-tree-sitter' THING without adding it to `meow-char-thing-table'."
    (when (stringp types)
      (setq types (list types)))
    (let* ((sym (intern (string-join types "/")))
           (inner (mapcar (lambda (type)
                            (intern (concat type ".inside")))
                          types))
           (outer (mapcar (lambda (type)
                            (intern (concat type ".around")))
                          types)))
      (meow-thing-register
       sym
       (lambda () (meow-tree-sitter--select-thing inner query))
       (lambda () (meow-tree-sitter--select-thing outer query)))))

  (defvar ii/meow-treesitter-table '((?a . "class")
                                     (?f . "function")
                                     (?y . "entry")
                                     (?, . "parameter")
                                     (?\; . "comment"))
    "Characters associated with `meow-tree-sitter' things.")

  (defun ii/meow-treesitter-setup-local-defaults ()
    "Register `meow-tree-sitter' defaults as buffer-local if local buffer has a treesit parser."
    (when (treesit-parser-list)
      (require 'meow-tree-sitter)
      (make-local-variable 'meow-char-thing-table)
      (dolist (bind ii/meow-treesitter-table)
        ;; delete
        (let* ((ch (car bind))
               (sym (intern (cdr bind)))
               (bind (cons ch sym)))
          (setq-local meow-char-thing-table
                      (assoc-delete-all (car bind)
                                        meow-char-thing-table))
          (cl-pushnew bind meow-char-thing-table)))))

  (setq meow-tree-sitter-can-expand t
        meow-tree-sitter-can-jump-forward t)
  :config
  ;; register `meow-treesitter' things without adding them to `meow-char-thing-table'.
  (dolist (bind ii/meow-treesitter-table)
    (let* ((types (let ((types (cdr bind)))
                    (if (stringp types)
                        (list types)
                      types)))
           (sym (intern (string-join types "/")))
           (inner (mapcar (lambda (type)
                            (intern (concat type ".inside")))
                          types))
           (outer (mapcar (lambda (type)
                            (intern (concat type ".around")))
                          types)))
      (meow-thing-register
       sym
       (lambda () (meow-tree-sitter--select-thing inner))
       (lambda () (meow-tree-sitter--select-thing outer)))))

  :hook
  (after-change-major-mode-hook . ii/meow-treesitter-setup-local-defaults))

(use-package surround
  :after meow
  :init
  (use-package-autoload-keymap
   'surround-keymap 'surround t)
  :bind*
  ( :map surround-keymap
    ("a" . surround-insert)))

;; operating on numbers
(use-package operate-on-number
  :straight ( :type git
              :host github
              :repo "knu/operate-on-number.el")
  :init
  (setq ii/operate-on-number-map (make-sparse-keymap))
  :bind-keymap*
  (("C-c e C-n" . ii/operate-on-number-map))
  :bind*
  ( :map ii/operate-on-number-map
    ("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . operate-on-number-at-point)
    ("C-u" . operate-on-number-read-operand)))

(use-package iedit)

(use-package meow
  :custom
  (meow-use-keypad-when-execute-kbd t)
  :init
  (setopt meow-use-dynamic-face-color nil
          meow-update-display-in-macro nil
          meow-use-clipboard t
          meow-esc-delay 0
          meow-expand-selection-type 'expand
          meow-use-cursor-position-hack t
          meow-pop-or-unpop-to-mark-repeat-unpop t
          meow-mode--set-explicitly nil
          meow-select-on-change nil)

  (defun ii/meow--line-numbers-toggle (&rest args)
    (when (bound-and-true-p display-line-numbers-mode)
      (pcase display-line-numbers-type
        ('relative (when meow-insert-mode
                     (setq-local display-line-numbers-type t)
                     (display-line-numbers--turn-on)))
        (_ (when (not meow-insert-mode)
             (setq-local display-line-numbers-type 'relative)
             (display-line-numbers--turn-on))))))

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

    (meow-define-state eat
      "meow state for `eat-mode'."
      :keymap meow-eat-state-map)

    (setq meow-cursor-type-eat 'bar)

    (meow-define-keys 'eat
      '("<escape>" . eat-self-input)
      '("M-SPC" . meow-keypad)
      '("C-g" . eat-self-input)
      (cons "C-w" meow-window-prefix-map)
      (cons "C-t" meow-eat-toggle-map))

    (meow-motion-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("C-s" . +search/buffer)
     (cons "C-w" meow-window-prefix-map))

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
     '("/" . "C-c /")
     '("]" . "C-c ]")
     '("[" . "C-c [")
     '("?" . +lookup/documentation)
     '("a" . "C-c a")
     '("b" . "C-c b")
     '("f" . "C-c f")
     '("g" . "C-c g")
     (cons "h" help-map)
     '("i" . "C-c i")
     '("j" . "C-c j")
     '("p" . "C-x p")
     '("t" . "C-c t")
     '("o" . "C-c o")
     '("q" . "C-c q")
     '("s" . "C-c s")
     (cons "w" meow-window-prefix-map))

    (meow-normal-define-key
     '("0" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("1" . meow-digit-argument)
     '("M-0" . meow-expand-0)
     '("M-9" . meow-expand-9)
     '("M-8" . meow-expand-8)
     '("M-7" . meow-expand-7)
     '("M-6" . meow-expand-6)
     '("M-5" . meow-expand-5)
     '("M-4" . meow-expand-4)
     '("M-3" . meow-expand-3)
     '("M-2" . meow-expand-2)
     '("M-1" . meow-expand-1)
     ;; '("~")
     ;; '("!" . ii/meow-shell-command)
     '("@" . "C-c @")
     (cons "#" ii/operate-on-number-map)
     ;; '("$" . ii/meow-pipe-to-shell-command)
     '("%" . meow-query-replace-regexp)
     ;; '("^")
     ;; '("&")
     ;; '("*")
     ;; '("\(")
     ;; '("\)")
     '("-" . negative-argument)
     '("_" . meow-join)
     '("=" . ii/meow-indent-region-or-buffer)
     ;; '("+" . )
     '(";" . meow-reverse)
     '(":" . ii/meow-command)
     '("," . ii/meow-inner-of-thing)
     '("." . ii/meow-bounds-of-thing)
     '("<" . meow-beginning-of-thing)
     '(">" . meow-end-of-thing)
     '("]" . "C-c ]")
     '("[" . "C-c [")
     '("}" . "M-}")
     '("{" . "M-{")
     '("'" . repeat)
     '("\"" . "C-x r")
     '("/" . +search/buffer)
     '("?" . +lookup/documentation)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-kill)
     '("D" . meow-kill-append)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("F" . meow-find-expand)
     '("g" . "M-g")
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
     '("m" . "C-c k")
     '("n" . meow-search)
     '("N" . meow-pop-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . +meow/yank)
     '("P" . meow-yank-pop)
     '("q" . "C-x C-k")
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     (cons "s" surround-keymap)
     '("t" . meow-till)
     '("T" . meow-till-expand)
     '("u" . undo)
     '("U" . undo-redo)
     '("v" . meow-visit)
     ;; '("V" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-save-append)
     '("z" . meow-pop-selection)
     '("Z" . meow-pop-all-selection))

    (meow-define-keys 'insert
      (cons "C-'" meow-normal-state-keymap))

    ;; kbd macro overrides
    (setq meow--kbd-kill-region "S-<delete>"
          meow--kbd-kill-ring-save "C-<insertchar>"
          meow-use-enhanced-selection-effect t
          meow-keypad-leader-dispatch "C-c"
          meow-keypad-ctrl-meta-prefix nil
          meow-keypad-meta-prefix nil
          meow-keypad-literal-prefix 32
          meow-keypad-execute-on-beacons t)

    (setf (alist-get 'meow-kill meow-selection-command-fallback)
          'meow-delete
          (alist-get 'meow-undo-in-selection meow-selection-command-fallback)
          'meow-undo)

    (setf (alist-get 'bounds meow-thing-selection-directions) 'forward
          (alist-get 'begining meow-thing-selection-directions) 'forward)

    (setopt meow-mode-state-list (append meow-mode-state-list
                                         '((pdf-view-mode . motion)
                                           (pdf-outline-buffer-mode . motion)
                                           (calibredb-search-mode . motion)
                                           (fundamental-mode . motion)
                                           (gfm-view-mode . motion)
                                           (ediff-mode . motion)
                                           (calibredb-show-mode . motion)
                                           (emms-playlist-mode . motion)
                                           (nov-mode . motion)
                                           (archive-mode . test)
                                           (vterm-mode . vterm)
                                           (speed-type-mode . insert)
                                           (shell-mode . insert)
                                           (eat-mode . eat)
                                           (shell-command-mode . motion)
                                           (comint-mode . insert))))

    (setq meow-char-thing-table '((?\( . round) (?\) . round)
                                  (?\[ . square) (?\] . square)
                                  (?\{ . curly) (?\} . curly)
                                  (103 . string)
                                  (101 . symbol) (119 . window) (98 . buffer) (112 . paragraph)
                                  (108 . line) (118 . visual-line) (100 . defun) (46 . sentence)))

    (meow-thing-register 'whitespace 'whitespace 'whitespace)
    (add-to-list 'meow-char-thing-table '(32 . whitespace))

    (meow-thing-register 'word 'word 'word)
    (add-to-list 'meow-char-thing-table '(?w . word))

    (meow-thing-register 'regexp-search
                         'ii/meow--regexp-search-inner
                         'ii/meow--regexp-search-bounds)

    (add-to-list 'meow-char-thing-table '(?\% . regexp-search))

    (meow-thing-register 'string-search
                         'ii/meow--search-inner
                         'ii/meow--search-bounds)


    (add-to-list 'meow-char-thing-table '(?\/ . search))

    (add-to-list 'meow-selection-command-fallback '(meow-reverse . negative-argument)))

  (defun +load-meow ()
    "Load `meow'."
    (require 'meow)
    (meow-setup)
    (meow-global-mode 1)
    (setq-default meow-replace-state-name-list '((eat . "<T>")
                                                 (normal . "<N>")
                                                 (motion . "<M>")
                                                 (keypad . "<K>")
                                                 (insert . "<I>")
                                                 (beacon . "<B>"))))
  :bind*
  (("M-SPC" . meow-keypad)
   ("M-<backspace>" . meow-backward-kill-symbol)
   ("C-o" . meow-pop-or-unpop-to-mark)
   ("C-`" . meow-pop-to-global-mark)
   ("M-g c" . meow-comment)
   ("C-c jf" . find-file-at-point)
   ("C-c twm" . menu-bar-mode)
   ("C-c twt" . tool-bar-mode)
   ("C-c tV" . visual-line-mode)
   ("C-c if" . insert-file)
   ("C-c ic" . insert-char)
   ("C-c ib" . insert-buffer)
   ("C-c b`" . meow-last-buffer)
   ("C-c kl" . meow-kmacro-lines)
   ("C-c km" . meow-kmacro-matches)
   :map emacs-lisp-mode-map
   ("C-c ib" . eval-print-last-sexp)
   :map help-map
   ("l" . load-library))
  :hook
  (meow-switch-state-hook . ii/meow--line-numbers-toggle)
  (after-init-hook . +load-meow))

(with-eval-after-load 'meow
  (require 'project)
  (defun +search/rg-thing-at-point ()
    (interactive)
    (let ((s (symbol-at-point)))
      (consult-ripgrep
       (or (project-root (project-current))
           default-directory)
       (if (eq s nil) "" (symbol-name s)))))

  (defun +search/affe-grep-thing-at-point (choose-dir)
    (interactive "P")
    (let ((default-directory (or (project-root (project-current))
                                 default-directory))
          (s (symbol-at-point)))
      (affe-grep (if choose-dir
                     (read-directory-name "Grep in: "
                                          default-directory)
                   default-directory)
                 (if (eq s nil) "" (symbol-name s)))))

  (defun +search/affe-find-thing-at-point (choose-dir)
    (interactive "P")
    (let ((default-directory (or (project-root (project-current))
                                 default-directory))
          (s (symbol-at-point)))
      (affe-find (if choose-dir
                     (read-directory-name "Find in: "
                                          default-directory)
                   default-directory)
                 (if (eq s nil) "" (symbol-name s)))))

  (defun +search/buffer ()
    "run this buffer's search function."
    (interactive)
    (command-execute +search-buffer-function)))

(bind-keys*
 ("M-g ." . +search/rg-thing-at-point)
 ("M-g ?" . +search/affe-grep-thing-at-point)
 ("M-g F" . +search/affe-find-thing-at-point)
 ([remap isearch-forward] . +search/buffer)
 ("C-c s." . +search/rg-thing-at-point)
 ("C-c s?" . +search/affe-grep-thing-at-point)
 ("C-c f." . +search/affe-find-thing-at-point))

(setopt find-program (+os/per-system! :win (shell-quote-argument "c:/Program Files/Git/usr/bin/find.exe")
                                      :linux "find"
                                      :wsl "find")
        grep-program (executable-find "grep")
        grep-find-ignored-directories '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist")
        grep-use-headings t
        isearch-wrap-pause 'no
        isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        search-default-mode t
        search-whitespace-regexp ".*"
        isearch-motion-changes-direction t
        isearch-regexp-lax-whitespace t
        isearch-repeat-on-direction-change t
        multi-isearch-pause t
        query-replace-skip-read-only t
        fringe-mode (cons 5 12)
        right-margin-width 1)

(use-package replace
  :straight nil
  :bind*
  (("C-c sr" . query-replace)
   ("C-c sg" . query-replace-regexp)))

(use-package rg
  :init
  (setq rg-align-position-numbers t)
  (defun ii/rg--setup ()
    (setq-local outline-regexp "File.*$"
                outline-heading-end-regexp "\n"))
  :bind*
  (("M-s /" . rg-isearch-project)
   ("C-c /'" . rg-dwim)
   ("C-c /p" . rg-project)
   ("C-c /s" . rg-isearch-project)
   ("C-c /S" . rg-isearch-current-dir)
   :map rg-mode-map
   ("i" . wgrep-change-to-wgrep-mode))
  :hook
  (rg-mode-hook . ii/rg--setup))

;; emacs >= 31 includes `grep-edit-mode'
(use-package wgrep
  :if (< emacs-major-version 30))


(use-package buffer-box
  :straight (buffer-box :type git
                        :host github
                        :repo "rougier/buffer-box"))

(use-package solaire-mode
  :hook
  (enable-theme-functions . solaire-global-mode))

(use-package doom-themes
  :demand
  :preface
  (add-to-list 'custom-theme-load-path (file-name-concat (expand-file-name straight-base-dir)
                                                         "straight"
                                                         "repos"
                                                         "doom-gruber-darker-theme/"))
  :init
  (defun ii/appearance-setup-doom-themes ()
    (progn
      (setq doom-themes-enable-bold nil
            doom-themes-enable-italic t
            doom-themes-treemacs-enable-variable-pitch nil
            doom-themes-treemacs-theme "doom-colors")
      (doom-themes-visual-bell-config)
      (load-theme +base/theme t)))
  :config
  (ii/appearance-setup-doom-themes)
  :hook
  (server-after-make-frame-hook . ii/appearance-setup-doom-themes))

(use-package rainbow-delimiters
  :hook ((prog-mode-hook emacs-lisp-mode-hook) . rainbow-delimiters-mode))

(use-package colorful-mode
  :init
  (setq colorful-use-prefix t
        colorful-allow-mouse-clicks nil)
  :bind*
  (("C-c tc" . colorful-mode))
  :hook
  ((css-mode-hook
    css-ts-mode-hook
    web-mode-hook
    help-mode-hook
    lsp-help-mode-hook
    helpful-mode-hook)
   . colorful-mode))

(use-package hl-todo
  :commands
  global-hl-todo-mode
  :init
  (with-eval-after-load (thread-first +base/theme
                                      (symbol-name)
                                      (concat "-theme")
                                      (intern))
    (setq hl-todo-keyword-faces
          `(("HOLD" . "#d0bf8f")
            ("TODO" . ,(doom-color 'green))
            ("NEXT" . "#dca3a3")
            ("THEM" . "#dc8cc3")
            ("PROG" . ,(doom-color 'dark-blue))
            ("OKAY" . ,(doom-color 'blue))
            ("DONT" . "#5f7f5f")
            ("FAIL" . compilation-error)
            ("DONE" . "#afd8af")
            ("NOTE" . "#d0bf8f")
            ("MAYBE" . "#d0bf8f")
            ("KLUDGE" . warning)
            ("HACK" . warning)
            ("TEMP" . warning)
            ("FIXME" . compilation-error)
            ("XXXX*" . compilation-error)))
    (global-hl-todo-mode 1)))

(use-package doom-gruber-darker-theme
  :straight (doom-gruber-darker-theme :type git
                                      :host github
                                      :repo "implicit-image/doom-gruber-darker-theme")
  :config
  ;; these faces are modified by meow and need to be overridden here
  (custom-set-faces `(region ((t (:extend nil))))
                    `(secondary-selection ((t (:extend nil))))))

;;;; Window Management
(use-package implicit-windows
  :straight `(implicit-windows :type nil
                               :local-repo ,(expand-file-name "windows" +init-module-path))
  :bind*
  ( :map meow-window-prefix-map
    ("\m" . +windows/toggle-maximize-window)
    ("C-v" . +windows/below-selected-prefix)
    ("C-b" . +windows/bottom-window-prefix)
    ("C-M-h" . +windows/left-vsplit-prefix)
    ("C-M-j" . +windows/below-hsplit-prefix)
    ("C-M-k" . +windows/above-hsplit-prefix)
    ("C-M-l" . +windows/right-vsplit-prefix)
    ("M-h" . +windows/left-side-window-prefix)
    ("M-j" . +windows/bottom-side-windows-prefix)
    ("M-k" . +windows/top-side-window-prefix)
    ("M-l" . +windows/right-side-window-prefix)
    ("C-c tM" . +windows/toggle-modeline)))

(use-package window
  :straight nil
  :init

  (setq window-repeat-map (make-sparse-keymap)
        switch-to-buffer-in-dedicated-window nil
        switch-to-buffer-obey-display-actions nil
        switch-to-buffer-preserve-window-point t
        display-buffer-alist
        '( ;; normal window without a modeline
          ((or . ((derived-mode . calibredb-search-mode)
                  (derived-mode . calibredb-edit-annotation-mode)
                  (derived-mode . calibredb-show-mode)))
           (display-buffer-same-window))
          ;; vc-diff
          ((or . ("\*vc-diff\*"))
           (display-buffer-use-some-window))
          ;;flymake
          ((or . ((derived-mode . flymake-project-diagnostics-mode)
                  (derived-mode . flymake-diagnostics-buffer-mode)))
           (display-buffer-in-side-window)
           (side . bottom)
           (window-height . 0.35))
          ((or . (("\*Warnings\*")))
           (display-buffer-no-window))
          ;; bottom side window
          ((or . ((derived-mode . rg-mode)
                  (derived-mode . grep-mode)
                  (derived-mode . xref--xref-buffer-mode)))
           (display-buffer-same-window)
           (dedicated . t)
           (post-command-select-window . t))
          ((or . ("\*Completions\*"
                  (derived-mode . completion-list-mode)))
           (display-buffer-below-selected)
           (window-parameters . ((mode-line-format . none)))
           (preserve-size . (t . t)))
          ;; popup bottom buffers
          ((or . ("\*Org Select\*"
                  "\*lsp-bridge-doc\*"
                  "\*lspce-hover\*"
                  "\*lsp-help\*"
                  "\*tide-documentation\*"
                  "\*eldoc\*"
                  (derived-mode . help-mode)
                  (derived-mode . lsp-ui-imenu-mode)
                  (derived-mode . apropos-mode)
                  (derived-mode . helpful-mode)))
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 0.4)
           (post-command-select-window . t))
          ;; shell command
          ((or . ((derived-mode . shell-command-mode)))
           (display-buffer-below-selected)
           (window-height . (body-lines . shrink-window-if-larger-than-buffer)))
          ;; embark shenanigans
          ((or . ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                  " *Embark Actions*"
                  (derived-mode . embark-collect-mode)))
           (display-buffer-no-window)
           (side . bottom)
           (window-parameters . ((mode-line-format . none)))
           (window-height . 0.3))
          ;; interactive bottom buffers
          ((or . ((derived-mode . flycheck-error-list-mode)
                  (derived-mode . comint-mode)
                  (derived-mode . compilation-mode)
                  (derived-mode . sly-repl-mode)
                  (derived-mode . lsp-treemacs-error-list-mode)
                  "\*Dictionary\*"
                  "\*LSP Lookup\*"
                  "\*vc-diff\**"))
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 0.4)
           (dedicated . t)
           (post-command-select-window . t))
          ;; left sidebar
          ((and . (" \*Treemacs-*"
                   (derived-mode . treemacs-mode)))
           (display-buffer-in-side-window)
           (preserve-size . t)
           (dedicated . t)
           (side . left))
          ;; right sidebar
          ((or . ((derived-mode . pdf-outline-buffer-mode)
                  "\*Call Hierarchy\*"))
           (display-buffer-in-side-window)
           (dedicated . t)
           (side . right))
          ;; some window
          ((or . ("*aider"))
           (display-buffer-in-side-window)
           (side . right))
          ;; top split
          ;; left split
          ;; right split
          ;; temp same window
          ((or . ("\*Org-Babel\*"
                  "\*Org Src\*"))
           (display-buffer-same-window)
           (dedicated . nil))
          ;; bottom side window
          ((or . ((derived-mode . proced-mode)))
           (display-buffer-same-window))))
  :bind*
  (("C-c b[" . previous-buffer)
   ("C-c b]" . next-buffer)
   ("C-c ]b" . next-buffer)
   ("C-c [b" . previous-buffer)
   ("C-c twd" . window-toggle-dedicated)
   :map meow-window-prefix-map
   ("C-o" . other-window-prefix)
   ("C-s" . same-window-prefix)
   ("-" . shrink-window)
   ("+" . enlarge-window)
   ("q" . delete-window)
   ("Q" . kill-buffer-and-window)
   ("v" . split-window-horizontally)
   ("s" . split-window-vertically)
   ("=" . balance-windows)
   ("O" . delete-other-windows)
   ("-" . shrink-window)
   ("+" . enlarge-window)
   ("M-\-" . shrink-window-horizontally)
   ("M-\+" . enlarge-window-horizontally)
   ("w" . other-window)
   :repeat-map window-repeat-map
   ("v" . split-window-horizontally)
   ("s" . split-window-vertically)
   ("w" . other-window)
   ("q" . delete-window)
   :exit
   ("=" . balance-windows)))

(use-package windmove
  :straight nil
  :init
  (setq windmove-wrap-around nil)
  :bind*
  ( :map meow-window-prefix-map
    ("h" . windmove-left)
    ("j" . windmove-down)
    ("k" . windmove-up)
    ("l" . windmove-right)
    ("H" . windmove-swap-states-left)
    ("J" . windmove-swap-states-down)
    ("K" . windmove-swap-states-up)
    ("L" . windmove-swap-states-right)
    ("C-h" . windmove-display-left)
    ("C-j" . windmove-display-down)
    ("C-k" . windmove-display-up)
    ("C-l" . windmove-display-right)
    ("C-f" . windmove-display-new-frame)
    ("C-t" . windmove-display-new-tab)
    ("C-d h" . windmove-delete-left)
    ("C-d j" . windmove-delete-down)
    ("C-d k" . windmove-delete-up)
    ("C-d l" . windmove-delete-right)
    :repeat-map window-repeat-map
    ("H" . windmove-swap-states-left)
    ("J" . windmove-swap-states-down)
    ("K" . windmove-swap-states-up)
    ("L" . windmove-swap-states-right)
    ("h" . windmove-left)
    ("j" . windmove-down)
    ("k" . windmove-up)
    ("l" . windmove-right)))

(use-package window-x
  :straight nil
  :bind*
  ( :map meow-window-prefix-map
    ("C-r r" . rotate-windows)
    ("C-r b" . rotate-windows-back)))

(use-package uniquify
  :straight nil
  :demand
  :config
  (setq uniquify-buffer-name-style 'forward))

;;;; Modeline

(setq mode-line-right-align-edge 'right-margin)

(add-hook 'minibuffer-mode-hook 'visual-line-mode)

(defvar ii/project-mode-line-format nil)

(defun ii/mode-line--get-project-name (project)
  "Get name of PROJECT while handling the cases of non-file buffers."
  (let ((bf (buffer-name)))
    (if (string-equal (project-name project) bf)
        ""
      (concat (project-name project) ":"))))

(defun ii/mode-line-update-project (&rest _args)
  "Update mode line project display."
  (setq-local ii/project-mode-line-format
              (if-let ((p (project-current nil default-directory)))
                  (propertize (ii/mode-line--get-project-name p) 'face 'success)
                "")))

(add-hook 'window-buffer-change-functions 'ii/mode-line-update-project)
(add-hook 'after-set-visited-file-name-hook 'ii/mode-line-update-project)

(setq-default header-line-format nil
              mode-line-format
              '((:eval meow--indicator)
                " "
                "%z%*%@ "
                (:eval ii/project-mode-line-format)
                mode-line-buffer-identification
                " "
                mode-line-position
                mode-line-format-right-align
                (lsp-mode lsp-modeline--diagnostics-string)
                (lspce-mode lspce--mode-line-format " ")
                (eglot--managed-mode (:eval (concat eglot-mode-line-format " ")))
                mode-line-process
                (flymake-mode flymake-mode-line-counters)
                (vc-mode vc-mode)
                " "
                mode-name))

(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      ;; for normal completion mechanism to not suck
      completion-show-help nil
      completion-show-inline-help nil
      completion-auto-select nil
      completion-cycling t
      completions-format 'one-column
      completions-max-height 10
      completions-header-format nil
      completions-detailed t
      completions-sort 'historical)

(use-package completion-preview
  :straight nil
  :init
  (setq completion-preview-minimum-symbol-length 5)
  :bind*
  (("C-c tp" . completion-preview-mode)
   :map completion-preview-active-mode-map
   ("M-n" . completion-preview-next-candidate)
   ("M-p" . completion-preview-prev-candidate)
   ("TAB" . completion-at-point)
   ("<tab>" . completion-at-point)
   ("M-TAB" . completion-preview-insert)))

(use-package minibuffer
  :straight nil
  :init
  (setq minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt)
        enable-recursive-minibuffers t)
  :bind*
  ( :map completion-in-region-mode-map
    ("RET" . minibuffer-choose-completion)
    ("M-n" . minibuffer-next-completion)
    ("M-p" . minibuffer-previous-completion))
  :hook
  (minibuffer-setup-hook . cursor-intangible-mode))

(setq dabbrev-upcase-means-case-search t
      dabbrev-ignored-buffer-modes '(archive-mode image-mode docview-mode pdf-view-mode tags-table-mode csv-mode))

(use-package marginalia
  :custom
  (marginalia-align 'center)
  (marginalia-align-offset 10)
  :hook
  (after-init-hook . marginalia-mode))

(use-package vertico
  :init
  (setopt vertico-count 12
          vertico-resize nil
          vertico-cycle t
          vertico-preselect 'first
          vertico-scroll-margin 5
          vertico-grid-max-columns 7
          vertico-buffer-display-action '((ii/vertico--buffer-mode-display-buffer)
                                          (reusable-frames . nil)
                                          (popup-frames . nil)
                                          (inhibit-switch-frame . t)))


  (defun ii/vertico--buffer-mode-display-buffer (buffer alist)
    "BUFFER_ ALIST."
    (let* ((windows (window-list nil -1 nil))
           (num-of-windows (length windows)))
      (cond ((with-current-buffer (window-buffer (minibuffer-selected-window))
               (eq major-mode 'vertico-buffer-mode))
             (message "recursive")
             (display-buffer-same-window buffer alist))
            ((one-window-p t)
             (let* ((props (cond ((>= (frame-pixel-width)
                                      (frame-pixel-height))
                                  '(:side right :height 1.0 :width 0.5))
                                 (t '(:side bottom :height 0.3 :width 1.0))))
                    (alist (append alist
                                   `((window-width . ,(plist-get props :width))
                                     (inhibit-same-window . t)
                                     (window-height . ,(plist-get props :height))
                                     (side . ,(plist-get props :side))
                                     (slot . 1)))))
               (display-buffer-in-side-window buffer alist)))
            ((> num-of-windows 3)
             (display-buffer-in-side-window buffer
                                            (append '((window-width . 1.0)
                                                      (window-height . 0.3)
                                                      (inhibit-same-window . t)
                                                      (side . bottom)
                                                      (slot . 1))
                                                    alist)))
            (t (display-buffer-use-some-window buffer alist)))))
  :config
  (vertico-multiform-mode)
  (setopt vertico-multiform-categories
          '((embark-keybinding grid))
          vertico-multiform-commands
          '((consult-grep buffer)
            (consult-ripgrep buffer)
            (consult-find buffer)
            (affe-grep buffer)
            (affe-find buffer)))
  :hook
  (marginalia-mode-hook . vertico-mode)
  (minibuffer-setup-hook . vertico-repeat-save)
  :bind*
  (("C-M-`" . vertico-suspend)
   ("C-c ss" . vertico-suspend)
   ("M-`" . vertico-repeat)
   :map vertico-map
   ("M-RET" . vertico-suspend)
   ("M-<return>" . vertico-suspend)
   ("M-j" . vertico-next)
   ("C-J" . vertico-next-group)
   ("M-k" . vertico-previous)
   ("C-K" . vertico-previous-group)
   ("C-c f" . vertico-flat-mode)
   ("C-c s" . vertico-suspend)
   ("C-c C-u" . vertico-directory-up)
   ("C-c ." . vertico-repeat)
   ("C-c i" . vertico-insert)))

(use-package embark-consult)

(use-package embark
  :custom
  (embark-mixed-indicator-delay nil)
  (embark-prompter 'embark-completing-read-prompter)
  (embark-quit-after-action t)
  (embark-indicators '(embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :bind*
  (("M-." . embark-dwim)
   ("M-'" . embark-act)
   :map embark-symbol-map
   ("h" . helpful-symbol)
   :map embark-variable-map
   ("h" . helpful-variable)
   :map embark-command-map
   ("h" . helpful-command)
   :map embark-function-map
   ("h" . helpful-function)
   :map vertico-map
   ("C-c C-e" . embark-export)
   ("C-c C-b" . embark-become)
   ("C-c C-l" . embark-live)
   ("C-c C-o" . embark-open-externally)
   ("C-c C-a" . embark-act)
   ("C-c C-d" . embark-dwim)
   ("C-c C-i" . embark-insert)
   ("C-c C-s" . embark-select)
   ("C-c C-c" . embark-collect)))

(use-package consult
  :autoload
  (consult--read)
  :custom
  (xref-show-xrefs-function 'consult-xref)
  :init
  (setq consult-find-args (+os/per-system! :wsl  (format "%s . -not ( -path */.[A-Za-z]* -prune )"
                                                         find-program)
                                           :linux (format "%s . -not ( -path */.[A-Za-z]* -prune )"
                                                          find-program)
                                           :win (format "%s . -not ( -path */.[A-Za-z]* -prune )"
                                                        find-program))
        consult-narrow-key "C-.")

  ;; setup preview for `find-file' and `project-find-file' commands
  (with-eval-after-load 'vertico
    (require 'consult)
    (setq read-file-name-function #'ii/consult-find-file-with-preview)

    (defun ii/consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
      (interactive)
      (let ((default-directory (or dir default-directory))
            (minibuffer-completing-file-name t))
        (consult--read #'read-file-name-internal
                       :state (consult--file-preview)
                       :prompt prompt
                       :require-match mustmatch
                       :predicate pred)))

    (setq project-read-file-name-function #'ii/consult-project-find-file-with-preview)

    (defun ii/consult-project-find-file-with-preview (prompt all-files &optional pred hist _mb)
      (let ((prompt (if (and all-files (file-name-absolute-p (car all-files)))
                        prompt
                      (concat prompt
                              (format " in %s"
                                      (consult--fast-abbreviate-file-name default-directory)))))
            (minibuffer-completing-file-name t))
        (consult--read (mapcar
                        (lambda (file)
                          (file-relative-name file))
                        all-files)
                       :state (consult--file-preview)
                       :prompt (concat prompt ": ")
                       :require-match t
                       :history hist
                       :category 'file
                       :predicate pred))))
  :config
  ;; disable previews for compilation errors
  (consult-customize
   consult-compile-error
   :preview-key nil
   consult-find
   :state (consult--file-preview))
  (add-to-list 'consult-preview-excluded-files "\.gpg")
  (add-to-list 'consult-preview-excluded-files "authinfo\.gpg")
  (add-to-list 'consult-preview-allowed-hooks 'window-buffer-change-functions)
  :bind*
  (("C-c f/" . consult-find)
   ("C-c fr" . consult-recent-file)
   ("C-c ir" . consult-register)
   ("C-c tm" . consult-minor-mode-menu)
   ("C-c bb" . consult-buffer)
   ("C-c bl" . consult-focus-lines)
   ("C-c bL" . consult-keep-lines)
   ("C-c jm" . consult-mark)
   ("C-c jM" . consult-global-mark)
   ("C-c jb" . consult-bookmark)
   ("C-c j\"" . consult-register)
   ("C-c j;" . consult-goto-line)
   ("C-c si" . consult-imenu)
   ("C-c sI" . consult-imenu-multi)
   ("C-c sb" . consult-line)
   ("C-c sB" . consult-line-multi)
   ("C-c so" . consult-outline)
   ("C-c se" . consult-flymake)
   ("C-c sc" . consult-compile-error)
   ("C-c //" . consult-ripgrep)
   ("C-c /f" . consult-find)
   :map help-map
   ("i" . consult-info)
   :map project-prefix-map
   ("b" . consult-project-buffer)
   :map vertico-map
   ("C-c C-h" . consult-history)
   ("M-s s" . consult-isearch-forward)
   ("M-s S-s" . consult-isearch-backward)))

(use-package affe
  :straight t
  :config
  (require 'orderless)

  (defun ii/affe-file-preview ()
    (let ((open (consult--temporary-files))
          (preview (consult--buffer-preview)))
      (lambda (action cand)
        (unless (eq action 'preview)
          (funcall open))
        (funcall preview action
                 (and cand
                      (eq action 'preview)
                      (funcall open cand))))))

  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))

  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler
        affe-count 1000)

  ;; Manual preview key for `affe-grep'
  (consult-customize affe-find
                     :state (ii/affe-file-preview)
                     :preview-key 'any
                     affe-grep
                     :preview-key '(:debounce 0.1 any))

  :bind*
  (("C-c /?" . affe-grep)
   ("C-c f?" . affe-find)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

;;;; Lookup
(setq max-mini-window-height 0.1
      browse-url-browser-function 'browse-url-firefox
      browse-url-firefox-program (+os/per-system! :wsl "/mnt/c/Program Files/Mozilla Firefox/firefox.exe"
                                                  :win "firefox.exe"
                                                  :linux "firefox")
      dictionary-server "dict.org"
      webjump-sites '(("DuckDuckGo" . [simple-query "https://duckduckgo.com" "www.duckduckgo.com/?q=" ""])
                      ("Google" . [simple-query "https://google.com" "www.google.com/search?q=" ""])
                      ("YouTube" . [simple-query "https://youtube.com/feed/subscriptions" "www.youtube.com/results?search_query=" ""])
                      ("Google" . [simple-query "https://google.com/search" "https://www.google.com/search?hl=en&q=" ""])
                      ("Stack Overflow" . [simple-query "https://duckduckgo.com/?q=site%3Astackoverflow.com+" "https://duckduckgo.com/?q=site%3Astackoverflow.com+" ""])
                      ("MyNixos" . [simple-query "https://mynixos.com" "https://mynixos.com/search?q=" ""])
                      ("Wikipedia" . [simple-query "https://en.wikipedia.org" "https://en.wikipedia.org/w/index.php?search=" ""])
                      ("CSS Tricks" . [simple-query "https://css-tricks.com" "https://css-tricks.com/?s=" ""])
                      ("Python docs" . [simple-query "https://docs.python.org/3" "https://docs.python.org/3/search.html?q=" ""])
                      ("DevDocs.io" . [simple-query "https://devdocs.io" "https://devdocs.io/#q=" ""])
                      ("Rust STD Docs" . [simple-query "https://doc.rust-lang.org/stable" "https://doc.rust-lang.org/stable/std/index.html?search=" ""])
                      ("Hoogle" . [simple-query "https://hoogle.haskell.org" "https://hoogle.haskell.org/?hoogle=" ""])
                      ("ChatGPT" . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""])))

(bind-key* "C-c jw" 'webjump)

(setq eldoc-echo-area-prefer-doc-buffer nil
      eldoc-idle-delay 0.1
      eldoc-print-after-edit nil
      eldoc-documentation-strategy 'eldoc-documentation-compose
      eldoc-echo-area-use-multiline-p nil
      eldoc-echo-area-display-truncation-message nil)

(use-package eldoc-box
  :init

  (defun ii/eldoc-box--position-function (width height)
    (let* ((pos (eldoc-box--default-at-point-position-function-1 width height))
           (x (car pos))
           (y (cdr pos)))
      (cons x
            y)))

  (setq eldoc-box-clear-with-C-g t
        eldoc-box-cleanup-interval 0.1
        eldoc-box-max-pixel-width (byte-compile-lambda
                                   '(lambda ()
                                      (if (display-graphic-p) 800 70)))
        eldoc-box-max-pixel-height (byte-compile-lambda
                                    '(lambda ()
                                       (if (display-graphic-p) 800 30)))
        eldoc-box-position-function 'ii/eldoc-box--position-function
        eldoc-box-doc-separator "\n-------------------------------\n"
        eldoc-box-only-multi-line nil
        eldoc-box-fringe-use-same-bg nil)

  (defun ii/eldoc-box--setup ()
    (if eldoc-box-mode
        (setq-local eldoc-idle-delay 0
                    eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
      (setq-local eldoc-idle-delay 0
                  eldoc-documentation-strategy 'eldoc-documentation-compose)))
  :config
  (setf (alist-get 'font eldoc-box-frame-parameters) +base/font-spec
        (alist-get 'outer-border-width eldoc-box-frame-parameters) 1
        (alist-get 'border-width eldoc-box-frame-parameters) 1)

  :bind*
  (("C-c te" . eldoc-box-hover-mode)
   ("C-c ck" . eldoc-box-help-at-point))
  :hook
  (eldoc-box-mode-hook . eldoc-box-hover-mode)
  (eldoc-box-mode-hook . ii/eldoc-box--setup))

(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        dumb-jump-prefer-searcher 'rg)
  :config
  ;; nwscript support
  ;; TODO: move to `nwscript-mode'.
  (with-eval-after-load 'nwscript-mode
    (add-to-list 'dumb-jump-find-rules
                 '( :type "function" :supports ("rg" "git-grep") :language "nwscript"
                    :regex "\\(struct +[A-Za-z0-9\_]+|int|void|float|object|itemproperty|effect|talent|location|command|action|cassowary|event|json|sqlquery|vector|string\\)[ \t]+\\([A-Za-z]+[A-Za-z_0-9]*\\)\\([ \t]*([^{]*)\\)[ \t\n]*{"
                    :tests ("void Func() {" "struct ps_effect Fire(struct test Test, int num)")
                    :not ("void Func();" "struct ps_test {" "struct ps_test\n{" "")))
    (add-to-list 'dumb-jump-find-rules
                 '( :type "type" :supports ("rg" "git-grep") :language "nwscript"
                    :regex "\\(struct +[A-Za-z0-9\_]+\\|int\\|void\\|float\\|object\\|itemproperty\\|effect\\|talent\\|location\\|command\\|action\\|cassowary\\|event\\|json\\|sqlquery\\|vector\\|string\\)[ \t]+\\(\\s+JJJ[ \t]*\\)\\(([A-Za-z0-9_, \n=]*)\\);"
                    :tests ("struct Parser {" "struct ps_effect {" "struct ps_test \n{" "struct Str \n{")
                    :not ("struct ps_effect PSTest()" "struct ")))
    (add-to-list 'dumb-jump-find-rules
                 '( :type "variable" :supports ("rg" "git-grep") :language "nwscript"
                    :regexp "\\b\\(const[ \t]+\\(int\\|float\\|string\\)[ \t]+\\)\\([A-Z0-9_]+\\)[ \t]+=.*"
                    :tests ("const int PS_VAR = 234;" "const string ps_str = \"fsfs\"")
                    :not ("int a = 213213;" "int PS_CONST_FUN"))))
  :hook
  (xref-backend-functions . dumb-jump-xref-activate))

(use-package xref
  :straight nil
  :init
  (setq xref-prompt-for-identifier nil
        xref-search-program 'ripgrep)
  :bind*
  (("M-g r" . xref-find-references)
   ("M-g R" . xref-find-references-and-replace)
   ("M-g d" . xref-find-definitions)
   ("M-g i" . xref-find-implementations)
   ("M-g D" . xref-find-declaration)
   ("M-g y" . xref-find-type-definition)
   ("C-c jr" . xref-find-references)
   ("C-c jd" . xref-find-definitions)
   :repeat-map xref-repeat-map
   ("," . xref-go-back)
   ("C-," . xref-go-forward)))

(defun ii/help--setup ()
  "Setup local lookup function."
  (setq-local +lookup-documentation-function 'helpful-at-point))

(add-hook 'help-mode-hook 'ii/help--setup)
(add-hook 'helpful-mode-hook 'ii/help--setup)

(use-package helpful
  :config

  (bind-keys* :map helpful-mode-map
              ("?" . +lookup/documentation))
  :bind*
  (([remap describe-symbol] . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-command] . helpful-command)
   ([remap describe-function] . helpful-function)
   ([remap describe-key] . helpful-key)
   :map helpful-mode-map
   ("?" . +lookup/documentation)
   :map help-map
   ("C-x" . helpful-command)
   ("C-v" . helpful-variable)
   ("C-f" . helpful-callable)
   ("C-k" . helpful-key)
   ("C-o" . helpful-symbol)
   ("C-!" . helpful-macro))
  :hook
  (helpful-mode-hook . rainbow-delimiters-mode))

(use-package help
  :straight nil
  :init
  (setq help-enable-completion-autoload nil
        help-enable-autoload nil
        help-enable-symbol-autoload nil)
  :config
  (bind-keys*
   :map help-mode-map
   ("x?" . +lookup/documentation))
  :bind
  ( :map help-map
    ("b" . describe-bindings)
    ("F" . describe-face)
    ("m" . describe-keymap)
    ("c" . describe-char)
    ("t" . describe-text-properties)
    ("\\" . describe-input-method)
    ("p" . describe-package)
    ("T" . describe-theme)
    ("M" . describe-mode)
    ("i" . info-lookup-symbol)
    ("w" . woman)))

;;;; Formatting
(setopt editorconfig-exclude-modes '(lisp-interaction-mode))
(add-hook 'after-init-hook 'editorconfig-mode)

(use-package apheleia
  :defer 5
  :hook
  (after-init-hook . apheleia-global-mode))

(use-package align
  :straight nil
  :bind*
  (("C-c ea" . align)))

(use-package vundo
  :custom
  (vundo-window-max-height 6)
  :bind*
  (("C-/" . vundo)
   :map vundo-mode-map
   ("r" . vundo-forward)
   ("q" . vundo-confirm)
   ("u" . vundo-backward)
   ("C-/" . vundo-backward)
   ("C-?" . vundo-forward)))

(setq show-paren-style 'parenthesis
      show-paren-delay 0.01
      show-paren-context-when-offscreen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(add-hook 'prog-mode-hook 'show-paren-local-mode)

(use-package window-stool
  :straight (window-stool :type git
                          :host github
                          :repo "JasZhe/window-stool")
  :init
  (setopt window-stool-n-from-top 4
          window-stool-n-from-bottom 0)

  (defmacro ii/window-stool--with-clear-buffer (buffer &rest body)
    "Evaluate BODY in BUFFER without `window-stool' overlay being present."
    (declare (indent defun))
    `(let* ((window (get-buffer-window ,buffer))
            (ov (alist-get window window-stool-overlays)))
       (when ov (delete-overlay ov))
       ,@body
       (window-stool-single-overlay window (window-start window))))

  :hook
  (prog-mode-hook . window-stool-mode)
  :bind*
  ("C-c tws" . window-stool-mode))

(use-package drag-stuff
  :bind
  (("M-k" . drag-stuff-up)
   ("M-j" . drag-stuff-down))
  :bind*
  ( :map prog-mode-map
    ("M-k" . drag-stuff-up)
    ("M-j" . drag-stuff-down)))

(use-package combobulate
  :straight (combobulate :type git
                         :host github
                         :nonrecursive t
                         :repo "mickeynp/combobulate")
  :config
  (setq combobulate-flash-node nil)
  :hook
  ((css-ts-mode-hook html-ts-mode-hook json-ts-mode-hook js-ts-mode-hook typescript-ts-mode-hook tsx-ts-mode-hook python-ts-mode-hook yaml-ts-mode-hook toml-ts-mode-hook go-ts-mode-hook) . combobulate-mode))

(setq treesit-font-lock-level 3
      set-mark-command-repeat-pop t
      backward-delete-char-untabify-method 'hungry)

;;;; Checkers
(use-package flyspell
  :straight nil
  :init
  (setopt flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil)

  (defun +flyspell/goto-prev-error (&optional next)
    (interactive "P")
    (flyspell-goto-next-error (not next)))

  (defun ii/flyspell-mode-deferred ()
    (+when-idle! 5.0 (flyspell-mode 1)))

  (defun ii/flyspell-prog-mode-deferred ()
    (+when-idle! 5.0 (flyspell-prog-mode)))

  :bind*
  (("C-c ]," . flyspell-goto-next-error)
   ("C-c [," . +flyspell/goto-prev-error)
   :repeat-map meow-error-repeat-map
   ("," . flyspell-goto-next-error)))

(add-hook 'prog-mode-hook 'ii/flyspell-prog-mode-deferred)
(add-hook 'org-mode-hook 'ii/flyspell-mode-deferred)
(add-hook 'markdown-mode-hook 'ii/flyspell-mode-deferred)
(add-hook 'prog-mode-hook 'flymake-mode)

(setq markdown-fontify-code-blocks-natively t)

(use-package flymake
  :straight nil
  :init
  (setq flymake-repeat-map (make-sparse-keymap))
  (setq flymake-show-diagnostics-at-end-of-line nil
        flymake-start-on-flymake-mode nil
        flymake-suppress-zero-counters nil
        flymake-indicator-type 'margins
        flymake-fringe-indicator-position nil
        flymake-margin-indicator-position 'right-margin
        flymake-margin-indicators-string '((error "●" compilation-error)
                                           (warning "●" compilation-warning)
                                           (note "●" compilation-info))
        flymake-mode-line-counter-format '("("
                                           flymake-mode-line-error-counter
                                           flymake-mode-line-warning-counter
                                           flymake-mode-line-note-counter
                                           ")"))

  (defvar ii/flymake--eol-hidden nil
    "TODO")

  (defun ii/flymake--get-overlays ()
    (cl-remove-if-not (lambda (ov)
                        (and (overlay-get ov 'flymake-overlay)
                             (not (overlay-get ov 'flymake--eol-overlay))))
                      (overlays-in (point-min) (point-max))))

  (defun ii/flymake--hide-eol-overlays ()
    (dolist (o (ii/flymake--get-overlays))
      (when (overlay-get o 'flymake--eol-overlay)
        (if-let* ((src-ovs (overlay-get o 'flymake-eol-source-overlays)))
            (overlay-put o 'display nil)))))

  (defun ii/flymake--show-eol-overlays ()
    (dolist (o (ii/flymake--get-overlays))
      (when (overlay-get o 'flymake--eol-overlay)
        (if-let* ((src-ovs (overlay-get o 'flymake-eol-source-overlays))
                  flymake-show-diagnostics-at-end-of-line)
            (overlay-put o 'display (flymake--eol-overlay-summary src-ovs))
          (delete-overlay o)))))

  (defun ii/flymake-toggle-eol ()
    (interactive)
    (+toggle-local-var! flymake-show-diagnostics-at-end-of-line 'normal nil)
    (when (bound-and-true-p flymake-mode)
      (flymake-start)
      (flymake--update-eol-overlays)
      (flymake--update-diagnostics-listings (current-buffer))))

  :config
  (bind-keys* :map flymake-prefix-map
              ("t" . ii/flymake-toggle-eol))
  :bind-keymap*
  ("C-c !" . flymake-prefix-map)
  :bind*
  ( :map flymake-mode-map
    ("M-g ]" . flymake-goto-next-error)
    ("M-g [" . flymake-goto-prev-error)
    ("C-c ]d" . flymake-goto-next-error)
    ("C-c [d" . flymake-goto-prev-error)
    ("C-c t!" . ii/flymake-toggle-eol)
    :map flymake-prefix-map
    ("]" . flymake-goto-next-error)
    ("[" . flymake-goto-prev-error)
    ("b" . flymake-show-buffer-diagnostics)
    ("p" . flymake-show-project-diagnostics)
    ("l" . flymake-switch-to-log-buffer)
    :repeat-map meow-error-repeat-map
    ("d" . flymake-goto-next-error)))

(use-package flymake-jsts
  :straight '(flymake-jsts :type git :host github :repo "orzechowskid/flymake-jsts" :branch "main"))

;;;; Snippets
(+when-idle! 3.0 (require 'yasnippet))
(+when-idle! 2.0 (require 'yasnippet-snippets))

(use-package yasnippet
  :init
  (setq doom-snippets-enable-short-helpers t
        yas-snippet-dirs (list (file-name-concat user-emacs-directory "straight" straight-build-dir "doom-snippets")
                               (file-name-concat user-emacs-directory "snippets")
                               (file-name-concat user-emacs-directory "straight" straight-build-dir "yasnippet-snippets/snippets")))
  :bind*
  (("C-TAB" . yasnippet-capf)
   ("C-<tab>" . yasnippet-capf)
   ("C-c is" . yas-insert-snippet))
  :hook
  ((prog-mode-hook fundamental-mode conf-mode-hook snippet-mode-hook text-mode-hook) . yas-minor-mode))

(use-package doom-snippets
  :straight (doom-snippets :type git
                           :host github
                           :repo "doomemacs/snippets"
                           :files ("*.el" "*"))
  :init
  (setq doom-snippets-enable-short-helpers t))

(use-package yasnippet-snippets
  :init
  (with-eval-after-load 'yasnippet
    (yasnippet-snippets-initialize)))

;;;; Compiling, running and debugging code

(use-package simple
  :straight nil
  :init
  (setopt next-error-recenter 4
          next-error-highlight t
          next-error-highlight-no-select t
          next-line-add-newlines nil)

  (defvar ii/undo--last-changes nil
    "Tadasdasd")

  (defvar ii/undo--current-change nil
    "Current change")

  (defun ii/undo--make-marker (pos buffer data)
    (let ((marker (make-marker)))
      (set-marker marker pos buffer)
      (list :marker marker :line (line-number-at-pos pos) :time (current-time :data data))))

  (defun ii/undo--advice (&rest args)
    (when (and (not (minibufferp))
               (not (bound-and-true-p dired-mode))
               (not buffer-read-only))
      (let ((change (nth 2 buffer-undo-list)))
        (when (not (equal change (car ii/undo--last-changes)))
          ;;(message "last change is %S" change);;
          (if change
              (cond ((and (listp change)
                          (length> change 2)
                          (markerp (car change)))
                     (add-to-list 'ii/undo--last-changes (ii/undo--make-marker (car change) (current-buffer))))
                    ((and
                      (listp change)
                      (stringp (car change)))
                     (add-to-list 'ii/undo--last-changes (ii/undo--make-marker (abs (cdr change)) (current-buffer))))
                    ((and (consp change)
                          (numberp (car change)))
                     (add-to-list 'ii/undo--last-changes (ii/undo--make-marker (abs (car change)) (current-buffer))))
                    (t nil)))))))

  ;; (defun ii/undo--setup ()
  ;;   (advice-add 'undo-auto--undoable-change :after 'ii/undo--advice))

  (defun ii/undo--consult ())

  (defun ii/undo-goto-last-change ())

  (defun ii/undo-goto-next-change ())

  (defun ii/undo--format-change-cand (change))

  (defun ii/undo-goto-change ()
    (interactive)
    (let ((change (consult--read (mapcar
                                  (lambda (marker-pair)
                                    (let ((marker (car marker-pair)))
                                      (list (format "%s:%d at %d" (buffer-name (marker-buffer marker))
                                                    (cdr marker-pair)
                                                    (marker-position marker))
                                            :marker marker
                                            :data
                                            :line (cdr marker-pair))))
                                  ii/undo--last-changes)
                                 :lookup (lambda (cand all-cands input narrow)
                                           (if-let* ((info (alist-get cand all-cands nil nil #'string-equal))
                                                     (marker (plist-get info :marker))
                                                     (line (plist-get info :line))
                                                     (buffer (marker-buffer marker)))
                                               (when t
                                                 (message "buffer name is %S" buffer)
                                                 (atomic-change-group
                                                   (pop-to-buffer buffer)
                                                   (with-current-buffer buffer
                                                     (goto-line marker))))
                                             (user-error "Cant find marker"))))))
      change))
  ;; (add-hook 'meow-global-mode-hook 'ii/undo--setup)
  :bind*
  ( ("C-x k" . kill-current-buffer)
    ("C-c bK" . kill-current-buffer)
    ("C-c bx" . scratch-buffer)
    ("C-c be" . next-error-select-buffer)
    ("C-c ]e" . next-error)
    ("C-c tr" . read-only-mode)
    ("C-c [e" . previous-error)
    ("C-c etc" . transpose-chars)
    ("C-c etw" . transpose-words)
    ("C-c etl" . transpose-lines)
    ("C-c ets" . transpose-sexps)
    ("C-c etp" . transpose-paragraphs)
    ("C-c et." . transpose-sentences)
    ("C-c etr" . transpose-regions)
    ("C-c t C-w w" . toggle-word-wrap)
    ("M-g ;" . ii/undo-goto-change)
    :repeat-map meow-error-repeat-map
    ("e" . next-error)
    (";" . negative-argument)))

(use-package comint
  :straight nil
  :init
  (setopt comint-eol-on-send t
          comint-prompt-read-only t)
  :bind*
  (("C-x C-!" . comint-run)))

(use-package compile
  :straight nil
  :init
  (setopt compilation-scroll-output t
          compilation-auto-jump-to-first-error nil
          compilation-max-output-line-length 500
          compilation-search-all-directories t
          compilation-context-lines t
          compilation-skip-threshold 0)

  :bind*
  ("C-x c" . compile)
  :hook
  (compilation-filter-hook . ansi-color-compilation-filter))

(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :config
  (fancy-compilation-mode 1))

(use-package shx)

(use-package dwim-shell-command
  :straight ( :type git
              :host github
              :repo "xenodium/dwim-shell-command")
  :config
  (require 'dwim-shell-commands))

(use-package dape
  :init
  (setq dape-key-prefix (kbd "C-c D")
        dape-buffer-window-arrangement 'right))

(use-package cape
  :hook
  (completion-at-point-functions . cape-file))

(use-package corfu
  :init
  (setq corfu-cycle t
        corfu-echo-delay nil
        corfu-auto-delay 0.1
        corfu-echo-mode nil
        corfu-preselect 'prompt
        corfu-preview-current 'insert
        corfu-auto nil
        corfu-popupinfo-delay '(0.5 . 0.4)
        corfu-left-margin-width 6
        corfu-right-margin-width 0
        corfu-bar-width 0
        corfu-count 17
        corfu-max-width 70
        corfu-min-width 60
        corfu-quit-no-match t
        corfu-on-exact-match 'insert
        global-corfu-test-minibuffer nil)

  :config

  (defun ii/corfu-toggle-auto ()
    (interactive)
    (when (bound-and-true-p corfu-mode)
      (+toggle-var! corfu-auto)
      (+toggle-var! corfu-preview-current t 'insert)
      (corfu-mode 1)))

  (setopt corfu--frame-parameters  '((no-accept-focus . t)
                                     (no-focus-on-map . t)
                                     (min-width . t)
                                     (min-height . t)
                                     (border-width . 1)
                                     (outer-border-width . 0)
                                     (internal-border-width . 0)
                                     (child-frame-border-width . 0)
                                     (vertical-scroll-bars . nil)
                                     (horizontal-scroll-bars . nil)
                                     (menu-bar-lines . 0)
                                     (tool-bar-lines . 0)
                                     (tab-bar-lines . 0)
                                     (tab-bar-lines-keep-state . t)
                                     (no-other-frame . t)
                                     (unsplittable . t)
                                     (undecorated . t)
                                     (cursor-type . nil)
                                     (no-special-glyphs . t)
                                     (desktop-dont-save . t)
                                     (inhibit-double-buffering . t)))

  (defvar ii/corfu-formatter-id-mapping
    '((array :str "[ ] " :face font-lock-type-face)
      (boolean :str "0|1 " :face font-lock-builtin-face)
      (class :str "cls " :face font-lock-type-face)
      (color :str "rgb " :face success)
      (command :str "cmd " :face default)
      (constant :str "cnst" :face font-lock-constant-face)
      (constructor :str "cons" :face font-lock-function-name-face)
      (enummember :str ""  :face font-lock-builtin-face)
      (enum-member :str "" :face font-lock-builtin-face)
      (enum :str "enum" :face font-lock-builtin-face)
      (event :str "evnt" :face font-lock-warning-face)
      (field :str "fld " :face font-lock-variable-name-face)
      (file :str "file" :face font-lock-string-face)
      (folder :str "dir " :face font-lock-doc-face)
      (interface :str "I   " :face font-lock-type-face)
      (keyword :str "key " :face font-lock-keyword-face)
      (macro :str "mac " :face font-lock-keyword-face)
      (magic  :str "mgc " :face font-lock-builtin-face)
      (method  :str "mtd " :face font-lock-number-face)
      (function :str "fun " :face font-lock-function-name-face)
      (module :str "mod " :face font-lock-preprocessor-face)
      (numeric :str "num " :face font-lock-builtin-face)
      (operator :str "op  " :face font-lock-comment-delimiter-face)
      (param :str "par " :face default)
      (property :str "prop" :face font-lock-variable-name-face)
      (reference :str "ref " :face font-lock-variable-name-face)
      (snippet :str "<s> " :face font-lock-string-face)
      (string :str "str " :face font-lock-string-face)
      (struct :str "{ } " :face font-lock-variable-name-face)
      (text :str "txt " :face font-lock-doc-face)
      (typeparameter :str "<T> " :face font-lock-type-face)
      (type-parameter :str "<T> " :face font-lock-type-face)
      (unit :str "unit" :face font-lock-constant-face)
      (value :str "val " :face font-lock-builtin-face)
      (variable :str "var " :face font-lock-variable-name-face)
      (t :str "    "  :face font-lock-warning-face)))

  (defun ii/corfu-margin-formatter (_)
    "Margin formatter for corfu."
    (and-let* ((kindfunc (plist-get completion-extra-properties :company-kind)))
      (lambda (cand)
        (let* ((kind (funcall kindfunc cand))
               (short (when-let* ((entry (or (alist-get (or kind t) ii/corfu-formatter-id-mapping)
                                             (alist-get t ii/corfu-formatter-id-mapping)))
                                  (face (plist-get entry :face))
                                  (str (plist-get entry :str)))
                        (propertize str 'face face))))
          (concat short "| ")))))

  (add-to-list 'corfu-margin-formatters 'ii/corfu-margin-formatter)
  :hook
  (after-init-hook . global-corfu-mode)
  (global-corfu-mode-hook . corfu-popupinfo-mode)
  (global-corfu-mode-hook . corfu-echo-mode)
  (global-corfu-mode-hook . corfu-history-mode)
  :bind*
  (("C-TAB" . corfu-complete)
   ("C-c ta" . ii/corfu-toggle-auto)
   :map corfu-map
   ("M-h" . corfu-popupinfo-documentation)
   ("M-g" . corfu-info-location)
   ("M-e" . corfu-expand)
   ("C-SPC" . corfu-insert-separator)
   ("C-<space>" . corfu-insert-separator)
   ([tab] . corfu-next)
   ("<tab>" . corfu-next)
   ("TAB" . corfu-next)
   ([backtab] . corfu-previous)
   ("<backtab>" . corfu-previous)
   ("S-TAB" . corfu-previous)))

(use-package corfu-terminal
  :straight (corfu-terminal :type git
                            :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :if (< emacs-major-version 31)
  :init
  (setq corfu-terminal-disable-on-gui t)
  :hook
  (global-corfu-mode-hook . corfu-terminal-mode))

(use-package lspce
  :straight `(lspce :type git
                    :host github
                    :repo "zbelial/lspce"
                    :files (:defaults
                            ,(pcase system-type
                               ('gnu/linux "lspce-module.so")
                               ('darwin "lspce-module.dylib")))
                    :pre-build ,(pcase system-type
                                  ('gnu/linux '(("cargo" "build" "--release") ("cp" "./target/release/liblspce_module.so" "./lspce-module.so")))
                                  ('darwin '(("cargo" "build" "--release") ("cp" "./target/release/liblspce_module.dylib" "./lspce-module.dylib")))))
  :init
  (setq lspce-prefix-map (make-sparse-keymap)
        lspce-server-programs '(("rust" "rust-analyzer" "")
                                ("python" "jedi-language-server" "")
                                ("python" "pylsp" "")
                                ("python" "basedpyright-langserver" "--stdio")
                                ("haskell" "haskell-language-server-wrapper" "lsp")
                                ("C" "ccls" "")
                                ("sh" "bash-language-server" "start")
                                ("go" "gopls" "")
                                ("nix" "nixd" "")
                                ("typescript" "typescript-language-server" "--stdio")
                                ("js" "typescript-language-server" "--stdio")
                                ("java" "jdtls" ""))
        lspce-enable-eldoc t
        lspce-eldoc-enable-hover t
        lspce-eldoc-enable-signature t
        lspce-enable-flymake t
        lspce-log-level 3
        lspce-completion-no-annotation nil
        lspce-send-changes-idle-time 0.8
        lspce-enable-imenu-index-function t
        lspce-modes-enable-single-file-root '(python-ts-mode typescript-ts-mode c-ts-mode)
        lspce-xref-append-implementations-to-definitions t
        lspce-call-hierarchy-show-position t)
  :bind*
  ( :map lspce-mode-map
    ("M-g a" . lspce-code-actions)
    ("M-s r" . lspce-rename)))

;;;; Eglot
(use-package eglot
  :straight (eglot :type built-in)
  :init
  (defun ii/eglot--setup (&rest _args)
    (when eglot-inlay-hints-mode
      (eglot-inlay-hints-mode -1))
    (setq-local eldoc-documentation-strategy 'eldoc-documentation-default))

  (setopt eglot-autoshutdown t
          eglot-extend-to-xref t
          eglot-sync-connect 1
          eglot-ignored-server-capabilities
          `(:documentHighlightProvider
            :inlayHintProvider
            :colorProvider))
  :config
  (setopt eglot-code-action-indications '(eldoc-hint))
  (defun ii/eglot-rename (&rest args)
    (interactive)
    (let ((case-fold-search nil))
      (command-execute 'eglot-rename )))
  :bind*
  ( :map eglot-mode-map
    ("M-g R" . ii/eglot-rename)
    ("C-c cr" . ii/eglot-rename)
    ("C-c cc" . eglot-show-call-hierarchy)
    ("C-c ct" . eglot-show-type-hierarchy)
    ("C-c cf" . eglot-format)
    ("C-c ca" . eglot-code-actions))
  :hook
  (eglot-inlay-hints-mode-hook . ii/eglot--setup))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c c"
        lsp-auto-configure nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-imenu t
        lsp-enable-completion t
        lsp-enable-file-watchers t
        lsp-enable-folding nil
        lsp-eldoc-render-all t
        lsp-enable-on-type-formatting nil
        lsp-enable-flymake t
        lsp-headerline-breadcrumb-enable nil
        lsp-completion-provider :none
        lsp-diagnostics-provider :flymake))

(use-package lsp-ui
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-peek-enable t
        lsp-ui-imenu-enable t
        lsp-ui-sideline-enable nil)
  :config
  (setf  (alist-get 'font lsp-ui-doc-frame-parameters) +base/font-spec)
  (setopt lsp-ui-sideline-show-diagnostics nil
          lsp-ui-sideline-ignore-duplicate nil))

(use-package lsp-pyright
  :custom (lsp-pyright-langserver-command "basedpyright")
  :init
  (setq lsp-pyright-basedpyright-inlay-hints-generic-types t))

(use-package lsp-haskell
  :after lsp-mode)

;;; Indent indicators
(use-package whitespace
  :straight nil
  :init
  (setq whitespace-global-modes '(not image-mode nov-mode pdf-view-mode markdown-mode gfm-mode org-mode latex-mode dired-mode csv-mode nxml-mode ess-mode diff-mode wdired-mode magit-mode magit-diff-mode)
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (space-mark 160 [164] [95])
                                      (newline-mark 10 [36 10])
                                      (tab-mark 9 [187 9] [92 9]))
        whitespace-style '(face tab-mark spaces page-delimiters trailing space-after-tab newline indentation))
  :bind*
  ("C-c tS" . whitespace-toggle-options)
  :hook
  (before-save-hook . whitespace-cleanup)
  (after-init-hook . global-whitespace-mode))

(use-package outline
  :straight nil
  :init
  (setopt outline-minor-mode-cycle nil
          outline-minor-mode-cycle-filter nil
          outline-minor-mode-highlight nil
          outline-blank-line t)

  (defvar ii/outline-minor-mode-ellipsis " ...v "
    "String used for hidden outline entries.")

  (defun ii/outline-minor-mode--set-elipsis (ellipsis)
    "Set ellipsis used to denote hidden entries to ELLIPSIS."
    (let* ((dtable (or buffer-display-table (make-display-table)))
           (face-offset (* (face-id 'shadow) (ash 1 22)))
           (value
            (vconcat (mapcar (lambda (c)
                               (+ face-offset c))
                             (string-trim-right ellipsis)))))
      (set-display-table-slot dtable 'selective-display value)
      (setq buffer-display-table dtable)))

  (defun ii/outline-minor-mode--setup ()
    "Setup outline minor mode local buffer settings."
    (ii/outline-minor-mode--set-elipsis ii/outline-minor-mode-ellipsis))

  :hook
  ((grep-mode-hook rg-mode-hook prog-mode-hook) . outline-minor-mode)
  (outline-minor-mode-hook . ii/outline-minor-mode--setup)
  :bind*
  ( :map outline-minor-mode-map
    ("M-TAB" . outline-cycle)
    ("C-c [@" . outline-previous-heading)
    ("C-c ]@" . outline-next-heading)
    :repeat-map outline-repeat-map
    ("TAB" . outline-cycle)))

(use-package indent-bars
  :straight (indent-bars :type git
                         :host github
                         :repo "jdtsmith/indent-bars")
  :commands
  indent-bars-mode
  :init
  (setq indent-bars-color '(highlight :face-bg t :blend 0.5)
        indent-bars-starting-column 0
        indent-bars-display-on-blank-lines t
        indent-bars-pattern "."
        indent-bars-no-descend-lists t
        indent-bars-display-on-blank-lines t
        indent-bars-no-descend-string t
        indent-bars-prefer-character t)

  (defun ii/indent-bars--maybe-turn-on ()
    (when (not (memq major-mode '(emacs-lisp-mode lisp-interaction-mode clojure-mode lisp-mode common-lisp-mode)))
      (indent-bars-mode)))
  (add-hook 'prog-mode-hook 'ii/indent-bars--maybe-turn-on))

(use-package project
  :straight (project :type built-in)
  :config
  (defun ii/project--root-finder (dir)
    "Integrate .git project roots."
    (let ((dotgit (and (setq dir (locate-dominating-file dir ".git"))
                       (expand-file-name dir))))
      (and dotgit
           (cons 'transient (file-name-directory dotgit)))))

  (defun ii/project-magit-status ()
    (interactive)
    (magit-status (project-root (project-current))))

  (defun ii/project-list-buffers-consult (_project _files-only)
    (interactive)
    (consult-project-buffer))

  (add-hook 'project-find-functions 'ii/project--root-finder)

  (setopt project-buffers-viewer 'ii/project-list-buffers-consult
          project-compilation-buffer-name-function 'project-prefixed-buffer-name
          project-vc-extra-root-markers '("package.json" "cargo.toml" ".+\.cabal"))

  (add-to-list 'project-switch-commands '(project-dired "Open root dir"))
  (add-to-list 'project-switch-commands '(ii/project-magit-status "Magit status" ?m))
  (add-to-list 'project-switch-commands '(affe-find "Fuzzy Find file" ?F))
  (add-to-list 'project-switch-commands '(eat-project "Eat" ?E))
  (add-to-list 'project-switch-commands '(affe-grep "Fuzzy find rx" ?/)))

(use-package direnv
  :if (+os/is-linux-p)
  :init
  (setq direnv-always-show-summary nil
        direnv-show-paths-in-summary nil)
  :hook
  (after-init-hook . direnv-mode))

(use-package envrc
  :disabled
  :if (executable-find "direnv")
  :bind-keymap*
  ("" . envrc-command-map)
  :hook
  (after-init-hook . envrc-global-mode))

(use-package docker
  :if (executable-find "docker"))

(use-package kele
  :if (executable-find "kubectl")
  :config
  (kele-mode 1))

;; Version Control
(setopt vc-make-backup-files nil
        vc-display-status t
        diff-font-lock-syntax nil
        diff-refine 'navigation
        diff-refine-nonmodified nil
        diff-font-lock-prettify nil
        ediff-shell (+os/per-system! :linux "sh"
                                     :wsl "sh")
        ediff-ignore-case t
        ediff-no-emacs-help-in-control-buffer t
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        vc-directory-exclusion-list (append (or (when (boundp 'vc-directory-exclusion-list)
                                                  vc-directory-exclusion-list)
                                                '())
                                            '("straight" "node_modules" "build")))

(use-package git-modes)

(use-package git-timemachine
  :bind*
  (("C-c g C-t t" . git-timemachine-toggle)
   :map git-timemachine-mode-map
   ("C-c g C-t b" . git-timemachine-blame)
   ("C-c g C-t n" . git-timemachine-show-next-revision)
   ("C-c g C-t p" . git-timemachine-show-previous-revision)))

(use-package magit
  :preface
  (setq magit-auto-revert-mode nil)
  :init
  (setq forge-add-default-bindings t
        magit-git-executable (+os/per-system! :wsl "git"
                                              :linux "git"
                                              :win "C:/Program Files/Git/cmd/git.exe")
        magit-diff-refine-hunk 'all
        magit-diff-highlight-hunk-region-functions
        '(magit-diff-highlight-hunk-region-dim-outside
          magit-diff-highlight-hunk-region-using-face)
        magit-diff-paint-whitespace nil
        magit-diff-paint-whitespace-lines nil)
  :hook
  (magit-process-find-password-functions . magit-process-password-auth-source)
  :bind*
  (("C-c gg" . magit-status)
   ("C-c gb" . magit-blame)
   ("C-c gd" . magit-diff)
   ("C-c gx" . magit-dispatch)
   ("C-c g C-i" . magit-gitignore-in-topdir)))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package diff-hl
  :init
  (setq diff-hl-show-hunk-function 'diff-hl-show-hunk-inline-popup
        diff-hl-show-hunk-inline-popup-hide-hunk t
        diff-hl-show-hunk-inline-popup-smart-lines nil
        diff-hl-draw-borders nil
        diff-hl-update-async t
        diff-hl-margin-symbols-alist '((insert . "+")
                                       (delete . "-")
                                       (change . "=")
                                       (unknown . "?")
                                       (ignored . "i"))
        diff-hl-reference-revision nil)
  :hook
  (dired-mode-hook . diff-hl-dired-mode)
  (global-diff-hl-mode-hook . diff-hl-margin-mode)
  (after-init-hook . global-diff-hl-mode))

;; Terminal Emulation

(setq eshell-banner-message "")

(use-package eat
  :init
  :config
  (setopt eat-shell (+os/per-system! :linux "zsh"
                                     :win "bash")
          eat-enable-blinking-text nil
          eat-enable-shell-prompt-annotation nil
          eat-minimum-latency 0.009)
  ;;; eat sets the terminfo directory to the straight build path, which contains only source and emacs Info files. This points it to the actual repo.
  (setq eat-term-terminfo-directory (expand-file-name "straight/repos/eat/terminfo" user-emacs-directory))
  :bind*
  (("C-c oe" . eat)
   :map project-prefix-map
   ("E" . eat-project)
   :map meow-eat-toggle-map
   ("l" . eat-line-mode)
   ("c" . eat-char-mode)
   ("s" . eat-semi-char-mode)
   ("e" . eat-emacs-mode)))

;;;; Buffer Management
(use-package autorevert
  :straight nil
  :config
  (defun ii/auto-revert--toggle-mode-in-buffer (window)
    "Setup `autorevert' in current buffer."
    (if-let* ((buffer-window (get-buffer-window (current-buffer)))
              (buffer-frame (window-frame buffer-window))
              (visible (frame-visible-p buffer-frame)))
        (progn (auto-revert-mode 1)
               (auto-revert-buffer (current-buffer)))
      (auto-revert-mode -1))
    (message "running in buffer %S, auto-revert-mode is %S" (current-buffer) auto-revert-mode))

  (defun ii/auto-revert--setup-buffer ()
    (add-hook 'window-buffer-change-functions 'ii/auto-revert--toggle-mode-in-buffer 90 t)
    (setq-local auto-revert-check-vc-info t))

  :hook
  ((pdf-view-mode-hook nov-mode-hook dired-mode-hook) . ii/auto-revert--setup-buffer))

(use-package ibuffer
  :straight nil
  :init
  (setq ibuffer-show-empty-filter-groups nil
        ibuffer-filtering-alist nil
        ibuffer-saved-filter-groups
        '(("default"
           ("org" (or
                   (mode . org-mode)
                   (name . "^\\*Org Src")
                   (name . "^\\*Org Agenda\\*$")))
           ("tramp" (name . "^\\*tramp.*"))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")
                     (name . "^\\*Warnings\\*$")
                     (name . "^\\*Shell Command Output\\*$")
                     (name . "^\\*Async-native-compile-log\\*$")
                     (name . "^\\*straight-")))
           ("ediff" (or
                     (name . "^\\*ediff.*")
                     (name . "^\\*Ediff.*")))
           ("dired" (or
                     (mode . dired-mode)
                     (mode . wdired-mode)))
           ("code" (predicate (lambda ()
                                (derived-mode-p 'prog-mode))))
           ("terminal" (or
                        (mode . term-mode)
                        (mode . shell-mode)
                        (mode . eat-mode)
                        (mode . vterm-mode)
                        (mode . eshell-mode)))
           ("help" (or
                    (name . "^\\*Help\\*$")
                    (name . "^\\*info\\*$")
                    (name . "^\\*helpful"))))))
  :config
  (if (< emacs-major-version 31)
      ;; redefine ibuffer size column to display human readable sizes
      (define-ibuffer-column size
        ( :name "Size"
          :inline t
          :header-mouse-map ibuffer-size-header-map)
        (let* ((size (file-size-human-readable (buffer-size)))
               (suffix (string-trim-left size "[0-9\.]+"))
               (face (pcase suffix
                       ("k" 'font-lock-type-face)
                       ("m" 'font-lock-keyword-face)
                       ("g" 'font-lock-string-face)
                       (_ 'font-lock-bracket-face))))
          (propertize size 'face face)))
    (setq ibuffer-human-readable-size t))

  (define-ibuffer-column mode
    ( :name "Mode"
      :inline t
      :header-mouse-map ibuffer-mode-header-map
      :props
      ( 'mouse-face 'highlight
        'keymap ibuffer-mode-name-map
        'help-echo "mouse-2: filter bu this mode"
        'face 'font-lock-keyword-face))
    (with-current-buffer buffer
      (format-mode-line mode-name nil nil (current-buffer))))

  (define-ibuffer-column filename-and-process
    ( :name "Filename/Process"
      :header-mouse-map ibuffer-filename/process-header-map
      :props
      ( 'face 'font-lock-string-face)
      :summarizer
      (lambda (strings)
        (setq strings (delete "" strings))
        (let ((procs 0)
              (files 0))
          (dolist (string strings)
            (when (get-text-property 1 'ibuffer-process string)
              (setq procs (1+ procs)))
            (setq files (1+ files)))
          (concat (cond ((zerop files) "No files")
                        ((= 1 files) "1 file")
                        (t (format "%d files" files)))
                  ", "
                  (cond ((zerop procs) "no processes")
                        ((= 1 procs) "1 process")
                        (t (format "%d processes" procs)))))))
    (let ((proc (get-buffer-process buffer))
          (filename (ibuffer-make-column-filename buffer mark)))
      (if proc
          (concat (propertize (format "(%s %s)" proc (process-status proc))
                              'font-lock-face 'italic
                              'ibuffer-process proc)
                  (if (> (length filename) 0)
                      (format " %s" filename)
                    ""))
        filename)))

  (define-ibuffer-filter project
      "Filter buffers by project"
    ( :description "current project"
      :reader
      (intern
       (completing-read "Filter by project: "
                        (mapcar (lambda (e)
                                  (expand-file-name (car e)))
                                project--list))))
    (with-current-buffer buf
      (when-let* ((p (project-current nil qualifier)))
        (memq buf (project-buffers p)))))

  :bind*
  (("C-c bi" . ibuffer)
   :map ibuffer-mode-map
   ("/p" . ibuffer-filter-by-project)
   ("/^" . ibuffer-pop-filter)))


(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude ".*[0-9a-z]+\.\\(png\\|jpg\\|jpeg\\|webp\\|svg\\|gif\\)")
  (add-to-list 'recentf-exclude ".*\.priv/.*"))

(recentf-mode 1)

(use-package files
  :straight nil
  :bind*
  ("C-c br" . revert-buffer))

(use-package files-x
  :straight nil
  :bind*
  (("C-c ilp" . modify-file-local-variable-prop-line)
   ("C-c flv" . modify-file-local-variable)))

(use-package dired
  :straight nil
  :init
  (setq dired-dwim-target t
        dired-listing-switches "-alh --group-directories-first"
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-kill-when-opening-new-dired-buffer nil
        dired-auto-revert-buffer t
        dired-create-destination-dirs 'ask
        dired-auto-revert-buffer 'dired-buffer-stale-p
        dired-omit-verbose nil
        dired-vc-rename-file t
        dired-clean-confirm-killing-deleted-buffers nil)
  :bind*
  (("M-g M-d" . dired-at-point)
   :map dired-mode-map
   ("e" . wdired-change-to-wdired-mode)
   ("-" . dired-up-directory)))

(use-package wdired
  :straight nil
  :init
  (setq wdired-allow-to-change-permissions t
        wdired-use-dired-vertical-movement 'sometimes)
  :bind*
  (:map wdired-mode-map
        ("C-c C-t" . wdired-toggle-bit)
        ("C-c C-s" . wdired-set-bit)))

(use-package diredfl
  :hook
  (dired-mode-hook . diredfl-mode))

(use-package dired-hacks
  :init
  (setq dired-filter-group-saved-groups
        `(("default")
          ("documents"
           (extension "pdf" "doc" "djvu" "epub" "docx" "mobi"))
          ("images"
           (extension "svg" "svgz" "gif" "jpg" "jpeg" "png" "tiff" "xbm" "xpm" "picon" "icon" "pict" "icon" "rgba" "xcf"))-
          ("markup"
           (extension "typ" "tex" "md" "org")))
        dired-filter-verbose nil)
  :bind*
  ( :map dired-mode-map
    ("TAB" . dired-subtree-toggle))
  :hook
  (dired-mode-hook . dired-filter-mode)
  (dired-mode-hook . dired-collapse-mode))

;;;; remote
(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))

(with-eval-after-load 'tramp
  ;; change sudo timeout
  (add-to-list 'tramp-connection-properties '(nil
                                              "session-timeout"
                                              "240")))

(use-package implicit-org
  :straight `(implicit-org :type nil
                           :local-repo ,(expand-file-name "org" +init-module-path))
  :commands
  (+org/rg-in-roam-notes)
  :autoload
  (+org-mode--jupyter-setup
   +org-babel--setup
   +org-template-fn--file-in-subdir)
  :hook
  (org-roam-mode . +org-roam-mode--setup))

(use-package org
  :init
  (setq org-confirm-babel-evaluate nil
        org-startup-indented nil
        org-pretty-entities t
        org-highlight-latex-and-related '(latex native)
        org-use-sub-superscripts "{}"
        org-inhibit-logging t
        org-startup-with-inline-images t
        org-image-actual-width t
        org-latex-preview-process-default 'dvipng
        org-latex-preview-process-precompiled t
        org-latex-preview-live-throttle 0.5
        org-latex-preview-live-debounce 0.5
        org-latex-preview-live t
        org-latex-preview-mode-display-live '(inline block edit-special)
        org-latex-preview-appearance-options '(:foreground "#f1f1f1" :background "#181818" :scale 2.0 :zoom. 1.0 :page-width 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))
        +org/agenda-file (substitute-in-file-name "$HOME/org/agenda/agenda.org")
        +org/tasks-file (substitute-in-file-name "$HOME/org/agenda/tasks.org")
        +org/journal-file (substitute-in-file-name "$HOME/org/journal.org")
        +org/metrics-file (substitute-in-file-name "$HOME/org/metrics.org")
        org-hide-emphasis-markers 1
        org-directory "~/org/"
        org-hide-macro-markers 1
        org-latex-packages-alist '(("" "color" t) ("" "tikz" t))
        org-confirm-babel-evaluate nil
        org-md-headline-style 'setext
        org-odt-preferred-output-format "doc"
        org-return-follows-link t
        ;; agenda
        org-log-done 'time
        org-log-into-drawer t
        org-refile-targets '(("archive.org" :maxlevel . 1)
                             ("tasks.org" :maxlevel . 1))
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;;;; tags
  (setq org-tag-alist
        '((:startgroup)
          (:endgroup)
          ("@errand" . ?E)
          ("@home" . ?H)
          ("@work" . ?W)
          ("agenda" . ?a)
          ("planning" . ?p)
          ("publish" . ?P)
          ("batch" . ?b)
          ("note" . ?n)
          ("idea" . ?i)))

;;;; capture templates
  (setq org-capture-templates
;;;; tasks
        `(("t" "Tasks / Projects")
;;;; add a task
          ("tt" "Task" entry (file+olp ,+org/tasks-file "TODOS")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
;;;; add next immidiate task
          ("tn" "Next Task" entry (file+headline +org/tasks-file "Tasks")
           "** NEXT %? \nDEADLINE: %t")
;;;; journal
          ("j" "Journal Entries")
;;;;
          ("jj" "Journal" entry
           (file+olp+datetree +org/journal-file)
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree +org/journal-file)
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)
          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree +org/journal-file)
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)
          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline +org/metrics-file "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (setq org-agenda-files `(,(expand-file-name "agenda/agenda.org" org-directory)
                           ,(expand-file-name "agenda/birthdays.org" org-directory)
                           ,(expand-file-name "agenda/habits.org" org-directory)
                           ,(expand-file-name "agenda/tasks.org" org-directory))
        org-agenda-skip-unavailable-files t
        org-agenda-breadcrumbs-separator ">"
        org-agenda-start-with-log-mode t
        org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))
  :config
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  :bind*
  (("C-c oa" . org-agenda))
  :hook
  (org-mode-hook . visual-line-mode)
  (org-mode-hook . org-indent-mode)
  (org-ode-hook . org-latex-preview-mode))

(use-package org-contrib)

(use-package org-appear
  :hook (org-mode-hook . org-appear-mode))

(use-package toc-org
  :hook
  (org-mode-hook . toc-org-mode))

(use-package org-special-block-extras
  :hook
  (org-mode-hook . org-special-block-extras-mode)
  :custom
  (o-docs-libraries
   '("~/org-special-block-extras/documentation.org")
   "The places where I keep my ‘#+documentation’"))

(use-package org-transclusion
  :after org)

(use-package org-roam
  :custom
  (org-roam-directory (substitute-in-file-name "$HOME/org/roam"))
  (org-roam-dailies-directory "daily")
  (org-roam-dailies-capture-templates '(("d" "default" entry
                                         "* %?"
                                         :target (file+head "%<%Y-%m-%d>.org"
                                                            "#+title: %Y=%m-%d\n"))))
  :init
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag))
        org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags))))
        org-roam-db-update-on-save t
        org-roam-completion-everywhere t
        org-roam-capture-templates
        `(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("b" "book notes" plain "%?"
           :if-new (file+head "booknotes/%^{filename}.org" "#+title: %^{title}\n")
           :unnarrowed t
           :jump-to-captured t)
          ("l" "learning" plain "%?"
           :target (file+head "%(+roam/template-fn--file-in-subdir \"/learning\")"
                              "#+title: %^{title}\n")
           :unnarrowed t)))
  :hook
  (org-roam-mode-hook . +org-roam-mode--setup)
  :bind*
  (("C-c nf" . org-roam-node-find)
   ("C-c nr" . org-roam-node-random)
   ("C-c nc" . org-roam-capture)
   ("C-c n*" . +org/rg-in-roam-notes)
   ("C-c ns" . org-roam-db-sync)))

(use-package ob
  :straight nil
  :init
  (setq org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-src-preserve-indentation nil
        org-babel-load-languages
        '((emacs-lisp . t)
          (C . t)
          (sql . t)
          (js . t)
          (dot . t)
          (org  .t)
          (latex . t)
          (scheme . t)
          (sass . t)
          (groovy . t)
          (java . t)
          (gnuplot . t)
          (clojure . t)
          (python . t)
          (R . t)
          (ocaml . t)))

  (defvar +org-babel-temp-dir (file-name-concat (expand-file-name user-emacs-directory) "+org-babel"))
  :config
  (add-to-list 'org-src-lang-modes (cons "nwscript" 'nwscript)))

(use-package jupyter
  :hook
  (org-mode-hook . +org-mode--jupyter-setup))

;;;; Books
(use-package calibredb
  :custom
  (calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (calibredb-library-alist '(("~/library")))
  (calibredb-search-page-max-rows 50)
  (calibredb-preferred-format "pdf")
  (calibredb-virtual-library-alist '(("Economics" . "economics")
                                     ("Theory" . "theory")
                                     ("Maths" . "maths")
                                     ("Computer Science" . "cs")))
  (calibredb-format-all-the-icons t)
  (calibredb-format-icons-in-terminal t)
  (calibredb-format-character-icons t)
  :init
  (setq calibredb-root-dir (expand-file-name "~/library"))
  :config
  (defun ii/calibredb--setup ()
    (setq-local +search-buffer-function 'calibredb-search-live-filter
                +lookup-documentation-function 'calibredb-toggle-view-at-point))
  (add-hook 'calibredb-search-mode-hook 'ii/calibredb--setup)
  :functions
  (calibredb-candidates)
  :bind*
  (("C-c oc" . calibredb)
   ("C-c oC" . calibredb-consult-read)))

(use-package org-noter
  :custom
  (org-noter-default-notes-file-names '("booknotes.org" "notes.org"))
  (org-noter-notes-search-path '("~/org/booknotes"))
  (org-noter-default-heading-title  "page $p$")
  (org-noter-auto-save-last-location t)
  (org-noter-kill-frame-at-session-end nil)
  (org-noter-always-create-frame nil)
  (org-noter-insert-selected-text-inside-note t)
  :bind*
  ( :map pdf-view-mode-map
    ("C-c nn" . org-noter)
    :map nov-mode-map
    ("C-c nn" . org-noter)
    :map djvu-mode-
    :map org-noter-doc-mode-map
    ("i" . org-noter-insert-note)
    ("i" . org-noter-insert-precise-note)
    ("C-c nq" . org-noter-kill-session)
    :map org-noter-notes-mode-map
    ("C-c nq" . org-noter-kill-session)))

(use-package pdf-tools
  :custom
  (pdf-outline-imenu-use-flat-menus t)
  :init
  (setopt org-format-latex-header "\\documentclass{article}\n[DEFAULT-PACKAGES]\n[PACKAGES]\n\\usepackage{xcolor}")
  :mode ("\\.pdf\\'" . pdf-tools-install)
  :hook
  (pdf-view-mode-hook . pdf-view-themed-minor-mode))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :init
  (defun ii/nov-setup-local-faces ()
    "Setup local faces in nov.el mode."
    (when (not variable-pitch-mode)
      (variable-pitch-mode 1)))
  :hook
  (nov-mode-hook . variable-pitch-mode))

(use-package citar
  :custom
  (citar-bibliography '("~/bib/references.bib")))

;;;; LLM integration

(defun ii/api-key-from-auth-source (host key-var &optional user)
  "Return api key for USER at HOST that might be stored in environment variable KEY-VAR."
  (or (getenv key-var)
      (plist-get
       (car (auth-source-search :host host
                                :user (or user "apikey")
                                :require '(:secret)))
       :secret)
      (user-error "No api key found either in %s or in authinfo.gpg" key-var)))

(defmacro ii/api-key-from-auth-source! (host key-var &optional user)
  "Return a function that returns an API key for USER at HOST that could be stored in KEY-VAR."
  `(lambda ()
     (or (getenv ,key-var)
         (plist-get
          (car (auth-source-search :host ,host
                                   :user ,(or user "apikey")
                                   :require '(:secret)))
          :secret)
         (user-error "No api key found either in %s or in authinfo.gpg" ,key-var))))

(defmacro ii/preload-api-keys! (&rest key-assocs)
  "Setup api keys from KEY-ASSOCS."
  `(progn ,@(mapcar (lambda (assoc)
                      (let ((host (car assoc))
                            (env-var (cadr assoc))
                            (user (cddr assoc)))
                        `(or (getenv ,env-var)
                             (when-let* ((secret (plist-get (car (auth-source-search
                                                                  :host ,host
                                                                  :user (or ,user "apikey")
                                                                  :require '(:secret)))
                                                            :secret))
                                         (key (if (functionp secret)
                                                  (encode-coding-string (funcall secret) 'utf-8)
                                                secret)))
                               (setenv ,env-var key))
                             (user-error "No api key found either in %s or in authinfo.gpg" ,env-var))))
                    key-assocs)))


(use-package minuet
  :straight t
  :config
  ;; You can use M-x minuet-configure-provider to interactively configure provider and model
  (setopt minuet-provider 'openai-compatible
          minuet-request-timeout 0.5
          minuet-add-single-line-entry t
          minuet-n-completions 2
          minuet-auto-suggestion-throttle-delay 0.3
          minuet-auto-suggestion-debounce-delay 0.5)

  (plist-put minuet-openai-compatible-options :end-point "https://openrouter.ai/api/v1/chat/completions")
  (plist-put minuet-openai-compatible-options :api-key (ii/api-key-from-auth-source "openrouter.ai" "OPENROUTER_API_KEY"))
  (plist-put minuet-openai-compatible-options :model "kwaipilot/kat-coder-pro:free")

  ;; Prioritize throughput for faster completion
  (minuet-set-optional-options minuet-openai-compatible-options :provider '(:sort "throughput"))
  (minuet-set-optional-options minuet-openai-compatible-options :max_tokens 200)
  (minuet-set-optional-options minuet-openai-compatible-options :top_p 0.9)
  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64)

  (defun ii/minuet-auto-block-suggestions ()
    (and (not buffer-read-only)
         meow-insert-mode
         (not (bolp))
         (looking-at-p "\s*$")))
  :bind*
  (("C-c at" . minuet-auto-suggestion-mode)
   ("C-c a TAB" . minuet-show-suggestion)
   :map minuet-active-mode-map
   ("M-n" . minuet-next-suggestion)
   ("M-p" . minuet-previous-suggestion)
   ("C-g" . minuet-dismiss-suggestion)
   ("C-TAB" . minuet-accept-suggestion-line)
   ("TAB" . minuet-accept-suggestion))
  :hook
  (minuet-auto-suggestion-block-functions . ii/minuet-auto-block-suggestions))

(use-package mcp
  :straight (mcp :host github
                 :type git
                 :repo "lizqwerscott/mcp.el"))

(use-package aidermacs
  :straight (aidermacs :type git
                       :host github
                       :branch "main"
                       :repo "MatthewZMD/aidermacs")
  :init
  (setopt aidermacs-default-model "openrouter/mistralai/mistral-7b-instruct:free"
          aidermacs-editor-model "openrouter/mistralai/mistral-7b-instruct:free"
          aidermacs-weak-model "openrouter/mistralai/mistral-7b-instruct:free"
          aidermacs-vterm-use-theme-colors t
          aidermacs-backend 'comint
          aidermacs-default-chat-mode 'architect)
  :config
  (ii/preload-api-keys!
   ("openrouter.ai" "OPENROUTER_API_KEY"))

  (add-to-list 'display-buffer-alist '((derived-mode . aidermacs-comint-mode)
                                       (display-buffer-use-some-window)))
  (aidermacs-setup-minor-mode)
  :custom
  (aidermacs-use-architect-mode t)
  :bind*
  (("C-c aa" . aidermacs-transient-menu)))

(use-package gptel
  :config
  (setq gptel-model 'kwaipilot/kat-coder-pro:free
        gptel-include-reasoning nil
        ii/gptel-openrouter (gptel-make-openai "OpenRouter"
                              :host "openrouter.ai"
                              :endpoint "/api/v1/chat/completions"
                              :stream t
                              :key #'gptel-api-key
                              :models '(moonshotai/kimi-dev-72b:free
                                        kwaipilot/kat-coder-pro:free
                                        deepseek/deepseek-r1-0528-qwen3-8b:free))
        gptel-backend ii/gptel-openrouter)
  :hook
  (gptel-mode-hook . visual-line-mode)
  :bind*
  (("C-c ag" . gptel-menu)
   ("C-c a C-g" . gptel)
   ("C-c ar" . gptel-rewrite)
   :map embark-region-map
   :package embark
   ("R" . gptel-rewrite)))

(use-package agent-shell
  :init
  (setq agent-shell-header-style 'text
        agent-shell-file-completion-enabled t
        agent-shell-show-welcome-message nil
        agent-shell-highlight-blocks t)
  :config
  (add-to-list 'whitespace-global-modes 'agent-shell-mode t))

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :init
  (setq claude-code-ide-diagnostics-backend 'flymake
        claude-code-ide-eat-preserve-position t
        claude-code-ide-terminal-backend 'eat)
  :config
  (claude-code-ide-emacs-tools-setup)
  :bind* ("C-c ac" . claude-code-ide-menu))

(use-package eca)

;;Media
(setq max-image-size 15.0
      image-use-external-converter t
      image-auto-resize 'fit-window)

(with-eval-after-load 'image
  (bind-keys* :map image-map
              ("r" . +utils/open-random-file-in-dir)))


;; mpd support
(when (executable-find "mpd")
  (require 'mpc)

  (defun ii/mpc-current-song ()
    (interactive)
    (if (not (null mpc-current-song))
        (let ((minibuffer-message-clear-timeout 2))
          (message mpc-current-song))))

  (setq mpc-browser-tags '(Album|Playlist)
        mpc-status-buffer-format '("%-5{Time} / %{Duration} %2{Disc--}%4{Track}" "%{Title}" "%{Album}"
                                   "%{Artist}" "%128{Cover}"))

  (bind-keys* ("C-c mm" . mpc)
              ("C-c mn" . mpc-next)
              ("C-c mp" . mpc-prev)
              ("C-c mt" . mpc-toggle-play)
              ("C-c mr" . mpc-toggle-repeat)
              ("C-c ms" . mpc-toggle-shuffle)
              ("C-c m." . ii/mpc-current-song)))

(use-package leetcode)

;;;; Language modes

(setq typescript-ts-mode-indent-offset 4
      go-ts-mode-indent-offset 4
      c-ts-mode-indent-offset 4
      java-ts-mode-indent-offset 4
      rust-ts-mode-indent-offset 4
      elixir-basic-offset 4
      mhtml-ts-mode-css-fontify-colors nil)

(add-to-list 'auto-mode-alist '("\\.cjs\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsonc*\\'" . json-ts-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-ts-mode))
(add-to-list 'auto-mode-alist '("\\/git-rebase-todo\\'" . conf-mode))

(use-package tide
  :init
  (setq tide-enable-xref t
        tide-imenu-flatten t
        tide-completion-detailed t)
  :hook (typescript-ts-mode-hook . (lambda ()
                                     (interactive)
                                     (tide-setup)
                                     (tide-hl-identifier-mode +1))))

(use-package web-mode
  :mode "\\.html\\'"
  :config
  (setq web-mode-enable-auto-expanding t
        web-mode-enable-css-colorization nil))

(use-package composer
  :hook (php-ts-mode-hook . composer))

(use-package scala-mode
  :init
  (setq scala-indent:step 4
        scala-indent:align-parameters t))

(use-package kotlin-mode
  :init
  (setq kotlin-tab-width 4))

(use-package haskell-ts-mode
  :custom
  (haskell-ts-font-lock-level 3)
  (haskell-ts-use-indent t)
  :mode "\\.hs\\|.lhs\\'"
  :init
  (setopt haskell-ts-ghci (executable-find "ghci"))
  :config
  (add-to-list 'treesit-language-source-alist
               '(haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1")))
  (unless (treesit-grammar-location 'haskell)
    (treesit-install-language-grammar 'haskell)))

(use-package elisp-mode
  :straight nil
  :init
  (add-to-list 'auto-mode-alist '("\\.el\.gz\\'" . emacs-lisp-mode))
  :config

  (defun ii/emacs-lisp--setup ()
    (setq-local compile-command "emacs --load-file "
                +lookup-documentation-function 'helpful-at-point))
  (add-hook 'emacs-lisp-mode-hook 'ii/emacs-lisp--setup)
  (add-hook 'lisp-interaction-mode-hook 'ii/emacs-lisp--setup)
  :bind*
  ( :map emacs-lisp-mode-map
    ("C-c C-j" . eval-print-last-sexp)
    ("C-c C-m" . emacs-lisp-macroexpand)))

(use-package sly
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package gleam-ts-mode
  :mode "\\.gleam\\'"
  :init
  (setq gleam-ts-indent-offset 4))

(use-package mix
  :hook (elixir-ts-mode-hook . mix-minor-mode))

(use-package ada-mode
  :init
  (setq ada-indent-use 4
        ada-indent-when 4))

(use-package elm-mode
  :init
  (setq elm-reactor-port 6969
        elm-indent-offset 4
        elm-format-on-save t))

(use-package lean4-mode
  :commands lean4-mode
  :straight (lean4-mode :type git :host github
                        :repo "leanprover-community/lean4-mode"
                        :files ("*.el" "data")))

(use-package init-nand2tetris
  :commands
  (jack-mode hdl-mode tst-mode vm-mode)
  :straight (init-nand2tetris :type git
                              :host github
                              :repo "Deng-Li3/emacs-nand2tetris"))

(use-package merlin
  :init
  (setq merlin-completion-with-doc t
        merlin-completion-dwim t)

  :hook
  (tuareg-mode-hook . merlin-mode))

(use-package merlin-eldoc
  :init
  (setq merlin-eldoc-max-lines 10
        merlin-eldoc-delimiter "  \n  "))

(use-package tuareg
  :init
  (setq tuareg-browser 'browse-url-firefox
        tuareg-default-indent 4
        tuareg-match-patterns-aligned t
        ;; opam
        tuareg-opam-insinuate t
        tuareg-opam-indent-basic 4))

(use-package cdlatex
  :hook
  (LaTeX-mode-hook . turn-on-cdlatex)
  (org-mode-hook . org-cdlatex-mode))

(when (executable-find "typst")
  (use-package typst-preview
    :straight (typst-preview :type git
                             :host github
                             :repo "havarddj/typst-preview.el"))

  (use-package typst-ts-mode
    :straight '(:type git :host codeberg :repo "meow_king/typst-ts-mode")
    :mode ("\\.typ\\'" . typst-ts-mode)))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package nwscript-mode
  :straight (nwscript-mode :type nil
                           :local-repo "/home/b/projects/nwscript-mode.el")
  :autoload (nwscript-mode)
  :mode "\\.nss\\'"
  :config
  ;; add base game includes
  (let ((file (substitute-in-file-name "$HOME/nwn2mods/game-includes")))
    (if (file-directory-p file)
        (add-to-list 'nwscript-include-dirs file))))


;; :straight (nwscript-mode :type git
;;                          :host github
;;                          :branch "master"
;;                          :repo "implicit-image/nwscript-mode.el"))

(use-package csv-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.2da\\'" . csv-mode))
  :config
  (defun ii/csv-mode--setup ()
    (cond ((string-equal (file-name-extension (buffer-file-name)) "2da")
           (save-mark-and-excursion
             (when (null csv-align-mode)
               (csv-align-mode 1))
             (goto-char (point-min))
             ;; find row with id 0
             (search-forward-regexp "^0 ")
             ;; go line up to header line
             (previous-line)
             ;; set csv header
             (csv-header-line t)
             (setq-local buffer-invisibility-spec nil)))
          (t nil)))
  (add-hook 'csv-mode-hook 'ii/csv-mode--setup))

(use-package crystal-mode
  :init
  (setq crystal-indent-level 4))

(use-package speed-type
  :init
  (setq speed-type-save-statistic-option 'always)
  :bind*
  (("C-c ott" . speed-type-text)
   ("C-c otb" . speed-type-buffer)
   ("C-c otr" . speed-type-region)
   :map speed-type-mode-map
   ("C-c C-p" . speed-type-pause)))

(use-package elfeed
  :init
  (setq elfeed-db-directory (expand-file-name ".elfeed" user-emacs-directory)
        elfeed-enclosure-default-dir user-emacs-directory))

;; no config required
(ii/packages! f dash ov embark-consult verb devdocs vlf yasnippet-capf realgud lsp-metals ob-sql-mode djvu forge org-contrib htmlize ox-rss org-roam-ui ement mastodon elfeed poly-markdown poly-R poly-org fsharp-mode zig-mode erlang cuda-mode powershell nushell-mode xenops auctex ocaml-ts-mode dune reason-mode solidity-mode lean-mode d-mode gdscript-mode nim-mode gpr-mode idris-mode vhdl-ts-mode vhdl-ext nasm-mode masm-mode fasm-mode riscv-mode mips-mode fstar-mode sharper shader-mode sln-mode csproj-mode robe otp edts ess purescript-mode dart-mode common-lisp-snippets geiser racket-mode clj-refactor clojure-snippets cider clojure-mode groovy-mode sbt-mode pyvenv)

;; load os-specific stuff
(+os/per-system!
 :win
 ;; setup path for executables
 (+add-directories-to-exec-path! "c:/ProgramData/chocolatey/bin/"
                                 "c:/ProgramData/mingw64/mingw64/bin/"
                                 "c:/Program Files/Git/cmd/"
                                 "c:/Program Files/Git/usr/bin/"
                                 "c:/Users/b/AppData/Local/Programs/MiKTeX/miktex/bin/x64")
 ;; windows seems to struggle with this default
 (prefer-coding-system 'utf-8)
 :wsl
 (add-to-list 'exec-path "/mnt/c/Program Files/Mozilla Firefox/"))
;; aliases

;;; init.el ends here;;
