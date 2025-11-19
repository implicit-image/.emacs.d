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
      use-package-compute-statistics t)

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
  :after dired
  :init
  (setq dired-async-skip-fast t
        dired-async-small-file-max 5000000)
  :config
  (dired-async-mode))

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
(bind-key "C-c t" meow-toggle-prefix-map)
(bind-key "C-c q" meow-quit-prefix-map)
(bind-key "C-c g" meow-vc-prefix-map)
(bind-key "C-w" meow-window-prefix-map)
(bind-key "C-c c" meow-code-prefix-map)

(setopt indent-tabs-mode nil)

(setq user-full-name "Błażej Niewiadomski"
      user-mail-address "blaz.nie@protonmail.com"
      visible-bell nil
      display-line-numbers-type 'relative
      truncate-lines 40
      hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil
      truncate-partial-width-windows t
      create-lockfiles nil
      x-stretch-cursor nil
      backup-inhibited t
      make-backup-files nil
      scroll-step 0
      scroll-conservatively 7
      scroll-preserve-screen-position t
      scroll-margin 7
      scroll-error-top-bottom t
      comment-multi-line t
      read-extended-command-predicate 'command-completion-default-include-p
      comment-empty-lines t
      lazy-highlight-initial-delay 0
      completion-ignore-case t
      sentence-end-double-space nil
      use-dialog-box nil
      use-file-dialog nil
      use-short-answers t
      read-answer-short t
      indicate-buffer-boundaries nil
      indicate-empty-lines nil
      word-wrap t
      tab-width 4
      delete-selection-save-to-register t
      next-screen-context-lines 0
      warning-minimum-level :error
      auto-window-vscroll nil
      save-place-file (expand-file-name "saveplace" user-emacs-directory)
      save-place-limit 600
      tab-always-indent 'complete
      tab-first-completion '
      desktop-dirname user-emacs-directory
      desktop-restore-frames nil
      desktop-restore-reuse-frames nil
      large-file-warning-threshold (* 30 1000 1000)
      desktop-modes-not-to-save '(fundamental-mode tags-table-mode image-mode pdf-view-mod nov-mode org-mode)
      global-mark-ring-max 32
      mark-ring-size 32
      fill-column 80
      duplicate-line-final-position -1
      duplicate-region-final-position -1
      proced-enable-color-flag t
      proced-auto-update-flag 'visible
      visible-cursor nil
      savehist-additional-variables
      '(kill-ring
        register-alist
        mark-ring
        global-mark-ring
        search-ring
        regexp-search-ring))

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
(blink-cursor-mode -1)
(tooltip-mode -1)
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
  :bind*
  (("M-RET" . recenter)
   ("M-<return>" . recenter)
   ("M-]" . forward-paragraph)
   ("M-[" . backward-paragraph)
   ("M-g g" . beginning-of-buffer)
   ("M-g e" . end-of-buffer)
   ("M-g f" . find-file-at-point)
   ("M-g W" . browse-url-at-point)
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
   ("C-x K" . kill-buffer)
   ("C-c bk" . kill-buffer))

  :hook
  ;; better to see all of the code
  ((window-setup-hook server-after-make-frame-hook) . +font--setup)
  ((help-mode-hook helpful-mode-hook lsp-ui-doc-hook) . visual-line-mode)
  ;; display line numbers in text-editing modes
  (prog-mode-hook . visual-line-mode)
  (visual-line-mode-hook . visual-wrap-prefix-mode)
  (prog-mode-hook . display-line-numbers-mode)
  (display-line-numbers-mode-hook . header-line-indent-mode)
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

(with-eval-after-load (intern (concat (symbol-name +base/theme) "-theme"))
  (custom-set-faces `(vertical-border ((t (:background ,(doom-color 'bg) :foreground ,(doom-color 'fg-alt) :width narrow))))
                    `(border ((t (:background ,(doom-color  'bg) :foreground ,(doom-color 'fg) :width narrow))))
                    `(internal-border ((t (:background ,(doom-color 'bg) :width narrow))))))

(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))
(set-display-table-slot standard-display-table
                        'truncation
                        (make-glyph-code ?⮐))

(setq which-key-popup-type 'side-window
      which-key-preserve-window-configuration t
      which-key-side-window-max-width 0.2
      which-key-idle-delay 0.6
      which-key-side-window-max-height 0.2
      which-key-idle-secondary-delay 0.05
      which-key-separator " "
      which-key-sort-order 'which-key-key-order-alpha
      which-key-side-window-slot 2
      which-key-max-display-columns 5
      which-key-prefix-prefix "[M] "
      which-key-add-column-padding 5
      which-key-show-remaining-keys t
      which-key-min-column-description-width 30)

(add-hook 'meow-global-mode-hook (defun ii/which-key--setup ()
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

;;;; meow
;; meow utilities and extensions
(use-package implicit-meow
  :straight `(implicit-meow :type nil
                            :local-repo ,(expand-file-name "meow" +init-module-path))
  :commands
  (+meow/command
   +meow/match
   +meow/yank
   +meow/grab
   ii/meow--setup-mark-on-insert-change
   ii/meow--next-change-callback
   ii/meow-switch-char-case
   ii/meow-switch-case
   ii/meow-toggle-case-region
   ii/meow-upcase-dwim
   ii/meow-downcase-dwim
   ii/meow-change-number-at-point
   ii/meow-increment-number-at-point
   ii/meow-decrement-number-at-point)
  :autoload
  (ii/meow--beacon-mode-setup)
  :preface
  (setq ii/meow-toggle-case-repeat-map (make-sparse-keymap)
        ii/meow-surround-map (make-sparse-keymap)
        ii/meow-surround-char-map (make-sparse-keymap))
  :init
  (advice-add 'meow-insert :after 'ii/meow--setup-mark-on-insert-change)
  (add-to-list 'insert-pair-alist '(?| ?|))
  (add-to-list 'insert-pair-alist '(?\\ ?\\))
  (add-to-list 'insert-pair-alist '(?/ ?/))
  (add-to-list 'insert-pair-alist '(?* ?*))
  (add-to-list 'insert-pair-alist '(?_ ?_))
  :config
  (add-to-list 'meow-selection-command-fallback '(ii/meow-toggle-case . ii/meow-toggle-char-case))
  :bind*
  (("C-c tc" . ii/meow-switch-case)
   ("M-g C-c" . ii/meow-switch-case)
   ("M-g ~" . ii/meow-toggle-case-region)
   ("M-g u" . ii/meow-downcase-dwim)
   ("M-g u" . ii/meow-upcase-dwim)
   ("C-c ei" . ii/meow-increment-number-at-point)
   ("C-c ed" . ii/meow-decrement-number-at-point)
   ("C-c e")
   :repeat-map ii/meow-toggle-case-repeat-map
   ("~" . ii/meow-toggle-case-region)
   ("C-c" . ii/meow-switch-case)
   ("~" . ii/meow-toggle-case-region)
   ("u" . ii/meow-downcase-dwim)
   ("U" . ii/meow-upcase-dwim))
  :hook
  (meow-beacon-mode-hook . ii/meow--beacon-mode-setup))

(setq kmacro-ring-max 16
      kmacro-counter-value-start 1)

(use-package meow-tree-sitter
  :init
  (defvar ii/meow-treesitter-table '((?a . "class")
                                     (?f . "function")
                                     (?t . "test")
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

(use-package meow
  :custom
  (meow-use-keypad-when-execute-kbd t)
  :init
  (setq meow-use-dynamic-face-color nil
        meow-update-display-in-macro nil
        meow-expand-selection-type 'expand
        meow-pop-or-unpop-to-mark-repeat-unpop t
        meow-mode--set-explicitly nil
        meow-select-on-change nil)

  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

    (meow-define-state vterm
      "meow state for `vterm-mode'"
      :keymap meow-vterm-state-map)

    (meow-define-state eat
      "meow state for `eat-mode'."
      :keymap meow-eat-state-map)

    (setq meow-cursor-type-eat 'hollow)
    (setq meow-cursor-type-vterm 'hollow)

    (meow-define-keys 'eat
      '("M-SPC" . meow-keypad)
      '("C-g" . eat-self-input)
      (cons "C-t" meow-eat-toggle-map))

    (meow-define-keys 'vterm
      '("M-SPC" . meow-keypad)
      '("C-g" . vterm--self-insert))

    (meow-motion-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("C-h" . "C-w h")
     '("C-j" . "C-w j")
     '("C-k" . "C-w k")
     '("C-l" . "C-w l")
     '("C-s" . +search/buffer)
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
     '("n" . "C-c n")
     '("p" . "C-x p")
     '("t" . "C-c t")
     '("o" . "C-c o")
     '("q" . "C-c q")
     '("s" . "C-c s")
     (cons "w" meow-window-prefix-map))

    (meow-normal-define-key
     '("@" . "C-c @")
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
     '("-" . negative-argument)
     '(":" . +meow/command)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("<" . meow-beginning-of-thing)
     '(">" . meow-end-of-thing)
     '("]" . "C-c ]")
     '("[" . "C-c [")
     '("}" . "M-}")
     '("{" . "M-{")
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-kill)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . "M-g")
     '("G" . +meow/grab)
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
     '("m" . +meow/match)
     '("M" . meow-join)
     '("n" . meow-search)
     '("N" . meow-pop-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . +meow/yank)
     '("P" . meow-yank-pop)
     '("q" . quoted-insert)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     (cons "s" ii/meow-surround-char-map)
     '("S" . ii/meow-surround-with-string)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . undo-redo)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("Z" . meow-pop-all-selection)
     '("'" . repeat)
     '("/" . +search/buffer)
     '("?" . +lookup/documentation)
     (cons "C-w" meow-window-prefix-map)
     '("C-r" . meow-redo)
     '("C-j" . windmove-down)
     '("C-k" . windmove-up)
     '("C-h" . windmove-left)
     '("C-l" . windmove-right))

    ;; kbd macro overrides
    (setq meow--kbd-kill-region "S-<delete>"
          meow--kbd-kill-ring-save "C-<insertchar>"
          meow--kbd-keyboard-quit "<escape>"
          meow-use-enhanced-selection-effect t
          meow-keypad-leader-dispatch "C-c"
          meow-keypad-ctrl-meta-prefix nil
          meow-keypad-meta-prefix nil
          meow-keypad-execute-on-beacons t)

    (setf (alist-get 'meow-kill meow-selection-command-fallback)
          'meow-delete)

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
                                           (shell-mode . insert)
                                           (eat-mode . eat)
                                           (shell-command-mode . motion))))

    (meow-thing-register 'whitespace 'whitespace 'whitespace)
    (add-to-list 'meow-char-thing-table '(32 . whitespace))

    (meow-thing-register 'xml-tag
                         '(pair-regexp ("<[^/>]+>") ("</[^/>]+>"))
                         '(pair-regexp ("<[^/>]+>") ("</[^/>]+>")))

    (add-to-list 'meow-char-thing-table '(?x . xml-tag))

    (meow-thing-register 'regexp-search
                         'ii/meow--regexp-search-inner
                         'ii/meow--regexp-search-bounds)

    (add-to-list 'meow-char-thing-table '(?\% . regexp-search))

    (meow-thing-register 'string-search
                         'ii/meow--search-inner
                         'ii/meow--search-bounds)

    (add-to-list 'meow-char-thing-table '(?/ . search))

    (meow-thing-register 'angled
                         '(pair ("<") (">"))
                         '(pair ("<") (">")))

    (add-to-list 'meow-char-thing-table '(?a . angled)))

  (defun +load-meow ()
    "Load `meow'."
    (require 'meow)
    (meow-setup)
    (meow-global-mode 1)
    (setq-default meow-replace-state-name-list '((eat . "<T>")
                                                 (vterm . "<T>")
                                                 (normal . "<N>")
                                                 (motion . "<M>")
                                                 (keypad . "<K>")
                                                 (insert . "<I>")
                                                 (beacon . "<B>")))
    (custom-set-faces `(secondary-selection
                        ((t (:background ,(doom-lighten (doom-color 'bg) 0.05)))))
                      `(region
                        ((t (:background ,(doom-lighten (doom-color 'bg) 0.10)))))))
  :bind*
  (("M-SPC" . meow-keypad)
   ("M-<backspace>" . meow-backward-kill-symbol)
   ("C-O" . meow-pop-to-global-mark)
   ("C-o" . meow-pop-or-unpop-to-mark)
   ("M-g c" . meow-comment)
   ("C-c jf" . find-file-at-point)
   ("C-c jW" . browse-url-at-point)
   ("C-c twm" . menu-bar-mode)
   ("C-c twt" . tool-bar-mode)
   ("C-c tV" . visual-line-mode)
   ("C-c if" . insert-file)
   ("C-c ic" . insert-char)
   ("C-c ib" . insert-buffer)
   ("C-c b`" . meow-last-buffer)
   :map emacs-lisp-mode-map
   ("C-c ib" . eval-print-last-sexp)
   :map help-map
   ("l" . load-library))
  :hook
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

(use-package grep
  :straight nil
  :init
  (setopt find-program (+os/per-system! :win (shell-quote-argument "c:/Program Files/Git/usr/bin/find.exe")
                                        :linux "find"
                                        :wsl "find")
          grep-program (executable-find "grep")
          grep-find-ignored-directories '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist")
          grep-use-headings t))

(setopt isearch-wrap-pause 'no
        isearch-lazy-count t
        lazy-count-prefix-format "(%s/%s) "
        search-default-mode t
        search-whitespace-regexp ".*"
        isearch-motion-changes-direction t
        isearch-regexp-lax-whitespace t
        isearch-repeat-on-direction-change t
        multi-isearch-pause t)

(use-package replace
  :straight nil
  :init
  (setopt query-replace-skip-read-only t)
  :bind*
  (("C-c sr" . query-replace)
   ("C-c sg" . query-replace-regexp)))

(setopt imenu-auto-rescan t
        imenu-use-popup-menu 'on-mouse)

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

(setopt fringe-mode (cons 6 6))

(use-package solaire-mode
  :hook
  (enable-theme-functions . solaire-global-mode))

(use-package doom-themes
  :demand
  :init
  (defun ii/appearance-setup-doom-themes ()
    (progn
      (add-to-list 'custom-theme-load-path (file-name-concat straight-base-dir
                                                             "straight"
                                                             straight-build-dir
                                                             "doom-gruber-darker-theme/"))
      (setq doom-themes-enable-bold t
            doom-themes-enable-italic t
            doom-themes-treemacs-enable-variable-pitch nil
            doom-themes-treemacs-theme "doom-colors")
      (doom-themes-visual-bell-config)
      (load-theme +base/theme t)))
  :config
  (ii/appearance-setup-doom-themes))

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

(setq text-scale-mode-step 1.1)

(bind-keys* ("C-=" . text-scale-increase)
            ("C--" . text-scale-decrease))

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
  :straight (doom-gruber-darker-theme :type nil
                                      :local-repo "/home/b/repos/doom-gruber-darker-theme"))
(setq pulse-iterations 1)

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
          ;; flymake
          ((or . ((derived-mode . flymake-project-diagnostics-mode)
                  (derived-mode . flymake-diagnostics-buffer-mode)))
           (display-buffer-in-side-window)
           (side . bottom)
           (window-height . 0.35))
          ((or . ("\*Warnings\*"))
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
           (display-buffer-at-bottom)
           (window-height . 15)
           (dedicated . t))))
  :bind*
  ;; display buffer prefixes
  (("C-c b[" . previous-buffer)
   ("C-c b]" . next-buffer)
   ("C-c ]b" . next-buffer)
   ("C-c [b" . previous-buffer)
   ;; other
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
   ;; to exit window action repeat map
   :exit
   ("=" . balance-windows)))

(setq window-divider-default-places 'right-only
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)

(add-hook 'window-setup-hook 'window-divider-mode)

(use-package windmove
  :straight nil
  :init
  (setq windmove-wrap-around nil)
  :bind*
  ( :map meow-window-prefix-map
    ("H" . windmove-swap-states-left)
    ("J" . windmove-swap-states-down)
    ("K" . windmove-swap-states-up)
    ("L" . windmove-swap-states-right)
    ("h" . windmove-left)
    ("j" . windmove-down)
    ("k" . windmove-up)
    ("l" . windmove-right)
    ;; buffer display prefixes
    ("C-h" . windmove-display-left)
    ("C-j" . windmove-display-down)
    ("C-k" . windmove-display-up)
    ("C-l" . windmove-display-right)
    ("C-f" . windmove-display-new-frame)
    ("C-t" . windmove-display-new-tab)
    ;; delete in direction
    ("C-d h" . windmmove-delete-left)
    ("C-d j" . windomove-delete-down)
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

(use-package winner
  :straight nil
  :hook
  (after-init-hook . winner-mode)
  :bind*
  (("C-c ]w" . winner-redo)
   ("C-c [w" . winner-undo)
   :repeat-map winner-repeat-map
   ("W" . winner-redo)
   ("w" . winner-undo)))

;;;; Modeline

(setq uniquify-buffer-name-style nil
      mode-line-right-align-edge 'right-margin)

(add-hook 'minibuffer-mode-hook 'visual-line-mode)

(defvar ii/project-mode-line-format nil)

(defun ii/mode-line--get-project-name (project)
  ""
  (let ((bf (buffer-name)))
    (if (string-equal (project-name project) bf)
        ""
      (concat (project-name project) "/"))))

(defun ii/mode-line-update-project (&rest _args)
  "Update mode line project display."
  (if-let* ((p (project-current nil default-directory)))
      (setq-local ii/project-mode-line-format (ii/mode-line--get-project-name p))
    (setq-local ii/project-mode-line-format "")))

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
                (flymake-mode flymake-mode-line-counters)
                (vc-mode vc-mode)
                (vc-mode " ")
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
  :hook
  (global-corfu-mode-hook . global-completion-preview-mode)
  :bind*
  ( :map completion-preview-active-mode-map
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

(use-package dabbrev
  :straight nil
  :init
  (setq dabbrev-upcase-means-case-search t
        dabbrev-ignored-buffer-modes '(archive-mode image-mode docview-mode pdf-view-mode tags-table-mode csv-mode)))

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
            (affe-grep buffer)
            (affe-find buffer)))
  :hook
  (marginalia-mode-hook . vertico-mode)
  :bind*
  (("M-`" . vertico-suspend)
   ("C-c ss" . vertico-suspend)
   :map vertico-map
   ("M-RET" . vertico-suspend)
   ("M-<return>" . vertico-suspend)
   ("M-j" . vertico-next)
   ("C-J" . vertico-next-group)
   ("M-k" . vertico-previous)
   ("C-K" . vertico-previous-group)
   ("C-c f" . vertico-flat-mode)
   ("C-c s" . vertico-suspend)
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
  :config
  ;; disable previews for compilation errors
  (consult-customize
   consult-compile-error
   :preview-key nil)
  :bind*
  (("C-c f/" . consult-find)
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
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))

  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler
        affe-count 1000)
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep
                     affe-find
                     :preview-key '(:debounce 0.1 any))

  :bind*
  (("C-c /?" . affe-grep)
   ("C-c f?" . affe-find)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

;;;; Lookup
(setq max-mini-window-height 0.1
      browse-url-firefox-program (+os/per-system! :wsl "/mnt/c/Program Files/Mozilla Firefox/firefox.exe"
                                                  :win "firefox.exe"
                                                  :linux "firefox")
      dictionary-server "dict.org")

(use-package webjump
  :straight nil
  :custom
  (webjump-sites '(("DuckDuckGo" . [simple-query "https://duckduckgo.com" "www.duckduckgo.com/?q=" ""])
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
                   ("ChatGPT" . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""]))))

(setq eldoc-echo-area-prefer-doc-buffer nil
      eldoc-idle-delay 0.05
      eldoc-documentation-strategy 'eldoc-documentation-compose
      eldoc-echo-area-use-multiline-p nil
      eldoc-echo-area-display-truncation-message nil)

(use-package eldoc-box
  :init
  (setq eldoc-box-clear-with-C-g t
        eldoc-box-cleanup-interval 0.2
        eldoc-box-position-function 'eldoc-box--default-at-point-position-function
        eldoc-box-doc-separator "\n--------------\n"
        eldoc-box-only-multi-line nil
        eldoc-box-fringe-use-same-bg nil)

  (defun ii/eldoc-box-toggle-position-function ()
    (interactive)
    (pcase eldoc-box-position-function
      ('eldoc-box--default-at-point-position-function (setopt eldoc-box-position-function 'eldoc-box--default-upper-corner-position-function))
      (t (setopt eldoc-box-position-function 'eldoc-box--default-at-point-position-function))))
  :bind*
  (("C-c te" . eldoc-box-hover-mode)
   ("C-c tE" . ii/eldoc-box-toggle-position-function))
  :hook
  (eldoc-box-mode-hook . eldoc-box-hover-mode))

(use-package dumb-jump
  :disabled
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
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
  (setq-local +lookup-documentation-function 'helpful-at-point))

(add-hook 'help-mode-hook 'ii/help--setup)
(add-hook 'helpful-mode-hook 'ii/help--setup)

(use-package helpful
  :config
  (setq helpful-max-highlight 20000)
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
   ("?" . +lookup/documentation))
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
(use-package editorconfig
  :straight nil
  :init
  (setopt editorconfig-exclude-modes '(lisp-interaction-mode))
  :hook
  (after-init-hook . editorconfig-mode))

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

(use-package paren
  :straight nil
  :init
  (setopt show-paren-style 'parenthesis
          show-paren-delay 0.05
          show-paren-context-when-offscreen 'overlay
          show-paren-when-point-inside-paren t
          show-paren-when-point-in-periphery t)
  :config
  (defvar +paren--extension-overlay nil)

  (defvar +paren--prefix-overlay nil)

  (defvar +paren--context-face `( :background ,(face-attribute 'header-line :background)
                                  :extend t :inherit which-func))

  (defun +paren--add-prefix-overlay (beg len)
    "BEG END."
    (+paren--delete-prefix-overlay)
    (setq +paren--prefix-overlay (make-overlay beg (+ beg len)))
    (overlay-put +paren--prefix-overlay 'priority
                 (+ show-paren-priority 10))
    (overlay-put +paren--prefix-overlay
                 'face 'header-line)
    ;; (overlay-put +paren--prefix-overlay 'window t)
    (add-hook 'post-command-hook '+paren--delete-prefix-overlay
              nil 'local))

  (defun +paren--delete-prefix-overlay ()
    (when +paren--prefix-overlay
      (delete-overlay +paren--prefix-overlay)
      (setq +paren--prefix-overlay nil)
      (remove-hook 'post-command-hook '+paren--delete-prefix-overlay 'local)))

  (defun +paren--delete-extension-overlay ()
    (when +paren--extension-overlay
      (delete-overlay +paren--extension-overlay)
      (setq +paren--extension-overlay nil)
      (remove-hook 'post-command-hook '+paren--delete-extension-overlay 'local)))

  (defun +paren--add-extension-overlay (beg end num)
    "BEG END."
    (+paren--delete-extension-overlay)
    ;; (message "adding extension overlay")
    (setq +paren--extension-overlay (make-overlay end (+ end 1)))
    (overlay-put +paren--extension-overlay 'priority (+ show-paren-priority 1000))
    (overlay-put +paren--extension-overlay 'face '(:extend t :inherit header-line))
    (overlay-put +paren--extension-overlay 'window (selected-window))
    (add-hook 'post-command-hook '+paren--delete-extension-overlay nil 'local))

  (defun +paren--overlay-function (text)
    "Show TEXT in an overlay at the top-left of the current window with nice face spec."
    (show-paren--delete-context-overlay)
    (let* ((beg (window-start))
           (loc-info (save-excursion
                       (goto-char beg)
                       (cons (line-end-position)
                             (line-number-at-pos))))
           (end (car loc-info))
           (num (cdr loc-info))
           (clean-text (replace-regexp-in-string "\n" " " text))
           (priority (+ show-paren-priority 999))
           (prefix-gap (string-match "\\w\\|\(" text))
           (stripped-text (string-trim-left clean-text)))
      (setq show-paren--context-overlay (make-overlay (+ beg prefix-gap) end))
      (overlay-put show-paren--context-overlay 'display stripped-text)
      (overlay-put show-paren--context-overlay 'priority
                   priority)
      (overlay-put show-paren--context-overlay
                   'face +paren--context-face)
      (overlay-put show-paren--context-overlay 'window (selected-window))
      (add-hook 'post-command-hook #'show-paren--delete-context-overlay
                nil 'local)
      (+paren--add-extension-overlay beg end num)))

  ;; use the custom function instead of the provided one to
  (advice-add 'show-paren--show-context-in-overlay :override '+paren--overlay-function)
  :hook
  (prog-mode . show-paren-local-mode))

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

(setq treesit-font-lock-level 2
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

  :bind*
  (("C-c ]," . flyspell-goto-next-error)
   ("C-c [," . +flyspell/goto-prev-error)
   :repeat-map meow-error-repeat-map
   ("," . flyspell-goto-next-error)
   ("\<" . +flyspell/goto-prev-error)))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flymake-mode)

(use-package flymake
  :straight nil
  :custom-face
  (flymake-end-of-line-diagnostics-face ((t (:background ,(color-lighten-name (doom-color 'bg) 2)))))
  :init
  (setq flymake-repeat-map (make-sparse-keymap))
  (setopt flymake-show-diagnostics-at-end-of-line 'short
          flymake-start-on-flymake-mode nil
          flymake-suppress-zero-counters nil
          flymake-fringe-indicator-position 'right-fringe
          flymake-margin-indicator-position 'right-margin
          flymake-margin-indicators-string '((error "!" compilation-error)
                                             (warning "$" compilation-warning)
                                             (note "i" compilation-info))
          flymake-mode-line-counter-format '("("
                                             flymake-mode-line-error-counter
                                             flymake-mode-line-warning-counter
                                             flymake-mode-line-note-counter
                                             ")"))
  (defun ii/flymake-toggle-eol ()
    (interactive)
    (when (bound-and-true-p flymake-mode)
      (+toggle-local-var! flymake-show-diagnostics-at-end-of-line 'short nil)
      (flymake-mode 1)))
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
    :map flymake-prefix-map
    ("]" . flymake-goto-next-error)
    ("[" . flymake-goto-prev-error)
    ("b" . flymake-show-buffer-diagnostics)
    ("p" . flymake-show-project-diagnostics)
    ("l" . flymake-switch-to-log-buffer)
    :repeat-map meow-error-repeat-map
    ("d" . flymake-goto-next-error)
    ("D" . flymake-goto-prev-error)))

(use-package flymake-jsts
  :straight '(flymake-jsts :type git :host github :repo "orzechowskid/flymake-jsts" :branch "main")
  :hook
  (flymake-mode-hook . flymake-jsts-eslint-enable))

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
    :repeat-map meow-error-repeat-map
    ("e" . next-error)
    ("E" . previous-error)))

(use-package comint
  :straight nil
  :init
  (setopt comint-eol-on-send t
          comint-prompt-read-only t)
  :bind*
  (("C-x C-!" . comint)))

(use-package compile
  :straight nil
  :init
  (setopt compilation-scroll-output t
          compilation-auto-jump-to-first-error nil
          compilation-max-output-line-length 500
          compilation-search-all-directories t
          compilation-context-lines t
          compilation-skip-threshold 0)
  :hook
  (compilation-filter-hook . ansi-color-compilation-filter)
  :bind*
  (("C-x c" . compile)))

(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :init
  (with-eval-after-load 'compile
    (fancy-compilation-mode)))

(use-package dape
  :init
  (setq dape-key-prefix (kbd "C-c D")
        dape-buffer-window-arrangement 'right))

(use-package implicit-capf
  :straight `(implicit-capf :type nil
                            :local-repo ,(expand-file-name "capf" +init-module-path))
  :autoload
  (+completion--in-region)
  :init
  (setq completion-in-region-function '+completion--in-region)
  :bind*
  (("C-c ta" . +corfu-toggle-auto))
  :hook
  (global-corfu-mode-hook . +global-corfu-mode--setup))

(use-package corfu
  :init
  (setq corfu-cycle t
        corfu-echo-delay nil
        corfu-auto-delay 0.2
        corfu-echo-mode nil
        corfu-preselect 'prompt
        corfu-preview-current 'insert
        corfu-auto nil
        corfu-popupinfo-delay '(0.5 . 0.4)
        corfu-left-margin-width 6
        corfu-right-margin-width 0
        corfu-bar-width 0
        corfu-count 17
        corfu-max-width 50
        corfu-min-width 40
        corfu-quit-no-match t
        corfu-on-exact-match 'insert
        global-corfu-minibuffer nil)
  :config
  (setopt corfu--frame-parameters  '((no-accept-focus . t)
                                     (no-focus-on-map . t)
                                     (min-width . t)
                                     (min-height . t)
                                     (border-width . 1)
                                     (outer-border-width . 0)
                                     (internal-border-width . 0)
                                     (child-frame-border-width . 1)
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
  :hook
  (after-init-hook . global-corfu-mode)
  (global-corfu-mode-hook . corfu-popupinfo-mode)
  :bind*
  (("C-TAB" . corfu-complete)
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

(with-eval-after-load 'corfu
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
      (t :str "----"  :face font-lock-warning-face)))

  (defun ii/corfu-margin--get-by-kind (kind)
    "Get margin display by kind KIND."
    (when-let* ((entry (or (alist-get (or kind t) ii/corfu-formatter-id-mapping)
                           (alist-get t ii/corfu-formatter-id-mapping)))
                (face (plist-get entry :face))
                (str (plist-get entry :str)))
      (propertize str 'face face)))

  (defun ii/corfu-margin-formatter (_)
    "Margin formatter for corfu."
    (and-let* ((kindfunc (plist-get completion-extra-properties :company-kind)))
      (lambda (cand)
        (let* ((kind (funcall kindfunc cand))
               (short (ii/corfu-margin--get-by-kind kind)))
          (concat short "| ")))))

  (add-to-list 'corfu-margin-formatters 'ii/corfu-margin-formatter))

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
      (eglot-inlay-hints-mode -1)))

  (setopt eglot-autoshutdown t
          eglot-extend-to-xref t
          eglot-ignored-server-capabilities
          `(:documentHighlightProvider
            :inlayHintProvider
            :codeLensProvider
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
    ("C-c ca" . eglot-code-actions)
    ("C-c c" . eglot-))
  :hook
  (eglot-inlay-hints-mode-hook . ii/eglot--setup))

;;; Indent indicators
(use-package whitespace
  :straight nil
  :init
  (setq whitespace-global-modes '(not image-mode markdown-mode gfm-mode org-mode latex-mode dired-mode csv-mode nxml-mode ess-mode diff-mode wdired-mode magit-mode magit-diff-mode)
        whitespace-display-mappings '((space-mark 32 [183] [46])
                                      (space-mark 160 [164] [95])
                                      (newline-mark 10 [36 10])
                                      (tab-mark 9 [187 9] [92 9]))
        whitespace-style '(face tab-mark spaces lines-char page-delimiters trailing space-after-tab newline indentation))
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
           (value (vconcat (mapcar (lambda (c)
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
  (setopt indent-bars-color '(highlight :face-bg t :blend 0.15)
          indent-bars-starting-column nil
          indent-bars-pattern "."
          indent-bars-width-frac 0.3
          indent-bars-pad-frac 0.0
          indent-bars-zigzag nil
          indent-bars-no-descend-lists t
          indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
          indent-bars-highlight-current-depth '(:blend 0.6)
          indent-bars-display-on-blank-lines t
          indent-bars-spacing 1
          indent-bars-color '(region :face-bg t :blend 0.2)
          indent-bars-no-descend-string t
          indent-bars-prefer-character nil)
  (defun ii/indent-bars--maybe-turn-on ()
    (when (not (memq major-mode '(emacs-lisp-mode
                                  lisp-interaction-mode
                                  clojure-mode
                                  lisp-mode
                                  common-lisp-mode)))
      (indent-bars-mode)))
  (add-hook 'prog-mode-hook 'ii/indent-bars--maybe-turn-on))


(use-package sideline-blame)

(use-package sideline
  :init
  (setq sideline-backends-right `((sideline-lsp . up))
        sideline-backend-delays '((sideline-lsp . 1))
        sideline-backends-right-skip-current-line nil
        sideline-truncate t
        sideline-force-display-if-exceeds nil
        sideline-order-right 'up
        sideline-format-right "%s"
        sideline-display-backend-name nil
        sideline-display-backend-format ""
        sideline-display-backend-type nil)
  :bind
  (("C-c ts" . sideline-mode))
  :hook
  (lsp-mode-hook . sideline-mode))


;; Project Management
(use-package implicit-project
  :straight `(implicit-project :type nil
                               :local-repo ,(expand-file-name "project" +init-module-path))
  :autoload
  (+project-magit-status)
  :commands
  (+project-list-buffers-consult
   +project-vterm)
  :bind*
  ( :map project-prefix-map
    ("V" . +project-vterm))
  :hook
  (project-find-functions . +project--root-finder))

(use-package project
  :straight (project :type built-in)
  :config
  (setopt project-buffers-viewer '+project-list-buffers-consult
          project-mode-line t
          project-mode-line-format '(:eval (concat (project-mode-line-format) "/"))
          project-compilation-buffer-name-function 'project-prefixed-buffer-name
          project-vc-extra-root-markers '("package.json" "cargo.toml" ".+\.cabal"))

  (add-to-list 'project-switch-commands '(project-dired "Open root dir"))
  (add-to-list 'project-switch-commands '(+project-magit-status "Magit status" ?m))
  (add-to-list 'project-switch-commands '(affe-find "Fuzzy Find file" ?F))
  (add-to-list 'project-switch-commands '(eat-project "Eat" ?E))
  (add-to-list 'project-switch-commands '(+project-vterm "Vterm" ?V))
  (add-to-list 'project-switch-commands '(affe-grep "Fuzzy find rx" ?/)))

(use-package direnv
  :if (+os/is-linux-p)
  :init
  (setq direnv-always-show-summary nil
        direnv-show-paths-in-summary nil)
  :hook
  (after-init-hook . direnv-mode))

;; Version Control
(setopt vc-make-backup-files nil
        vc-display-status t
        diff-font-lock-syntax 'hunk-only
        diff-refine 'font-lock
        diff-refine-nonmodified t
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
   ("C-c gB" . magit-blame)
   ("C-c gD" . magit-diff)
   ("C-c gX" . magit-dispatch)))

(use-package diff-hl
  :init
  (setq diff-hl-show-hunk-function 'diff-hl-show-hunk-inline-popup
        diff-hl-showhunk-inline-popup-hide-hunk t
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

;;;; Terminal Emulation

(use-package vterm
  :if (executable-find "gcc")
  :init
  (setopt vterm-shell (getenv "SHELL")
          vterm-max-scrollback 2500)
  :bind*
  (("C-c ov" . vterm))
  :hook
  (vterm-mode-hook . hl-line-mode))

(use-package eat
  :config
  (setopt eat-shell (+os/per-system! :linux "bash"
                                     :win "bash"))
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
  :init
  (defun ii/auto-revert--toggle-mode-in-buffer (window)
    "Setup `autorevert' in current buffer."
    (with-current-buffer (current-buffer)
      (let* ((frame (selected-frame)))
        (if (frame-visible-p frame)
            (progn (auto-revert-mode 1)
                   (auto-revert-buffer (current-buffer)))
          (auto-revert-mode -1)))
      (message "running in buffer %S, auto-revert-mode is %S" (current-buffer) auto-revert-mode)))

  (defun ii/auto-revert--setup-buffer ()
    (add-hook 'window-buffer-change-functions 'ii/auto-revert--toggle-mode-in-buffer 1 t)
    (setq-local auto-revert-check-vc-info t))
  :hook
  ((pdf-view-mode-hook nov-mode-hook dired-mode-hook) . ii/auto-revert--setup-buffer))

(use-package ibuffer
  :straight nil
  :init
  (setq ibuffer-show-empty-filter-groups nil
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

(use-package recentf
  :straight nil
  :config
  (add-to-list 'recentf-exclude ".*[0-9a-z]+\.\\(png\\|jpg\\|jpeg\\|webp\\|svg\\|gif\\)")
  (add-to-list 'recentf-exclude ".*\.priv/.*")
  :hook
  (after-init-hook . recentf-mode)
  :bind*
  (("C-c fr" . recentf)))

(use-package files
  :straight nil
  :bind*
  ("C-c br" . revert-buffer))

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
        wdired-allow-to-change-permissions t
        wdired-use-dired-vertical-movement 'sometimes)
  :bind*
  (("M-g M-d" . dired-at-point)
   :map dired-mode-map
   ("C-e" . wdired-change-to-wdired-mode)
   ("i" . wdired-change-to-wdired-mode)
   ("-" . dired-up-directory)))

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
           (extension "typ" "tex" "md" "org")))))
;; :hook
;; (dired-mode-hook . dired-filter-mode))

;;;; remote
(use-package auth-source
  :straight nil
  :demand
  :init
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")))

(use-package tramp
  :straight nil
  :config
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
  :hook
  (org-mode-hook . visual-line-mode)
  (org-mode-hook . org-indent-mode)
  (org-ode-hook . org-latex-preview-mode))

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
    :map org-noter-doc-mode-map
    ("i" . org-noter-insert-note)
    ("i" . org-noter-insert-precise-note)
    ("C-c nq" . org-noter-kill-session)
    :map org-noter-notes-mode-map
    ("C-c nq" . org-noter-kill-session)))

(use-package reader
  :disabled
  :straight
  (cond ((and (not (file-directory-p "/etc/nixos"))
              (+os/is-linux-p))
         '(reader :type git
                  :host codeberg
                  :repo "divyaranjan/emacs-reader"
                  :files ("*.el" "render-core.so")
                  :pre-build ("make" "all")))
        (t t)))

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
  :hook
  (nov-mode-hook . variable-pitch-mode))

;;;; LLM integration

(use-package minuet
  :straight t
  :config
  ;; You can use M-x minuet-configure-provider to interactively configure provider and model
  (setq minuet-provider 'openai-fim-compatible)

  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 64))
;; :bind
;; (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
;;  ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
;;  ("C-c m" . #'minuet-configure-provider)
;;  :map minuet-active-mode-map
;;  ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
;;  ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
;;  ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
;;  ("M-A" . #'minuet-accept-suggestion) ;; accept whole completion
;;  ;; Accept the first line of completion, or N lines with a numeric-prefix:
;;  ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
;;  ("M-a" . #'minuet-accept-suggestion-line)
;;  ("M-e" . #'minuet-dismiss-suggestion)))

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
  (setq +llm-aidermacs-models (list :default ""))
  (setq aidermacs-default-model "openrouter/"
        aidermacs-editor-model "openrouter/"
        aidermacs-weak-model "" )
  :config
  (aidermacs-setup-minor-mode)
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet")
  :bind*
  (("C-c aa" . aidermacs-transient-menu)))

(use-package gptel
  :config
  (setq gptel-model 'DeepSeek-R1
        +gptel-openrouter (gptel-make-openai "OpenRouter"
                            :host "openrouter.ai"
                            :endpoint "/api/v1/chat/completions"
                            :stream t
                            :key #'gptel-api-key
                            :models '(moonshotai/kimi-dev-72b:free
                                      deepseek/deepseek-r1-0528-qwen3-8b:free))
        gptel-backend +gptel-openrouter)
  :hook
  (gptel-mode-hook . visual-line-mode)
  :bind*
  (("C-c ag" . gptel-menu)
   ("C-c ar" . gptel-rewrite)
   :map embark-region-map
   ("R" . gptel-rewrite)))

;;Media
(use-package image
  :straight nil
  :init
  (setopt max-image-size 15.0
          image-use-external-converter t
          image-auto-resize 'fit-window)
  :config
  (bind-keys* :map image-map
              ("r" . +utils/open-random-file-in-dir)))

(use-package emms
  :if (executable-find "mpd")
  :init
  (setq emms-player-list '(emms-player-mpd)
        emms-source-file-default-directory (getenv "XDG_MUSIC_DIR"))
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost"
        emms-player-mpd-server-port "6600"
        emms-player-mpd-music-directory (substitute-in-file-name "$XDG_MUSIC_DIR"))
  (emms-player-mpd-connect)
  :hook
  (emms-info-functions . emms-info-mpd)
  :bind*
  (("C-c mm" . emms)
   ("C-c mr" . emms-toggle-repeat-playlist)
   ("C-c mR" . emms-toggle-repeat-track)))


;;;; Language modes

(setq typescript-ts-mode-indent-offset 4
      go-ts-mode-indent-offset 4
      c-ts-mode-indent-offset 4
      java-ts-mode-indent-offset 4
      rust-ts-mode-indent-offset 4)

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
  (haskell-ts-ghci "ghci")
  :mode "\\.hs\\|.lhs\\'"
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

(use-package elixir-ts-mode
  :straight nil
  :init
  (setq elixir-basic-offset 4))

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
  :mode "\\.nss\\'")
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

;; no config required
(ii/packages! f dash ov embark-consult verb devdocs vlf yasnippet-capf realgud cape lsp-metals ob-sql-mode djvu forge org-contrib htmlize ox-rss org-roam-ui ement mastodon elfeed poly-markdown poly-R poly-org fsharp-mode zig-mode erlang markdown-mode cuda-mode powershell nushell-mode xenops auctex ocaml-ts-mode dune reason-mode solidity-mode lean-mode d-mode gdscript-mode nim-mode gpr-mode idris-mode nasm-mode masm-mode fasm-mode riscv-mode mips-mode fstar-mode sharper shader-mode sln-mode csproj-mode robe otp edts ess purescript-mode dart-mode common-lisp-snippets geiser racket-mode clj-refactor clojure-snippets cider clojure-mode groovy-mode sbt-mode pyvenv)


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


;;; init.el ends here;;
