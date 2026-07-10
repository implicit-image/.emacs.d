;;; -*- lexical-binding: t -*-
;; set up straight.el

;;; Code:

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
      native-comp-async-on-battery-power nil
      ;; lazy load by default
      use-package-always-defer t
      use-package-ignore-unknown-keywords (not init-file-debug)
      use-package-compute-statistics init-file-debug
      use-package-expand-minimally (not init-file-debug))

;; early org mode declaration to make sure the correct version gets loaded
(use-package org
  :straight `(org
              :fork ( :host nil
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


(use-package project
  :defer t
  :straight (:type built-in))

(defvar ii/elisp-path (expand-file-name "elisp" user-emacs-directory))

(add-to-list 'load-path ii/elisp-path)

(defvar +per-machine-config-feature (intern (concat "implicit-" (system-name))))

(defvar ii/load-on-focus-loss-list nil)

;; add everything to `load-path'
(dolist (path (directory-files (expand-file-name "elisp" user-emacs-directory)
                               t
                               "[^\.]+"))
  (add-to-list 'load-path path))
;;; Commentary:
;;

(require 'implicit-config-lib)

(ii/when-idle! 5.0
  (+set-env-vars-from-shell "PATH" "JAVA_HOME" "TERM" "EDITOR"))

;; smart tab
(defvar ii/smart-tab-skip-chars '(?\( ?\) ?{ ?} ?\[ ?\] ?\" ?\" ?\' ?\' ?\`)
  "Chars to skip instead of trying to indent.")

(defun ii/smart-tab (arg)
  "Try to call `completion-at-point'. If it cant complete, skip next \
character if it is in `ii/smart-tab-skip-chars', otherwise call `indent-for-tab-command' \
with ARG."
  (interactive "P")
  (cond
   ((region-active-p)
    (funcall-interactively #'indent-region (region-beginning) (region-end)))
   ((bolp)
    (indent-for-tab-command arg))
   (t
    (let ((char (char-after)))
      (or (completion-at-point)
          (cond ((and (memq char ii/smart-tab-skip-chars)
                      (> (point) (line-beginning-position)))
                 (forward-char arg))
                (t
                 (funcall-interactively 'indent-for-tab-command arg))))))))

(bind-key [remap indent-for-tab-command] #'ii/smart-tab global-map)

;;;; Elisp Libraries
(use-package async
  :init
  (setq dired-async-skip-fast t
        dired-async-small-file-max 5000000)
  (with-eval-after-load 'dired
    (dired-async-mode 1)))

(defun ii/hide-modeline ()
  "Hide current buffer's modeline."
  (if mode-line-format
      (setq-local mode-line-format nil)))

;;;; base Config
(setq meow-leader-global-map (make-sparse-keymap)
      meow-eat-state-map (make-sparse-keymap)
      meow-view-state-map (make-sparse-keymap)
      meow-ghostel-state-map (make-sparse-keymap)
      meow-ediff-state-map (make-sparse-keymap)
      meow-eat-toggle-map (make-sparse-keymap)
      meow-mc-global-map (make-sparse-keymap)
      meow-error-repeat-map (make-sparse-keymap)
      flymake-prefix-map (make-sparse-keymap)
      next-defun-repeat-map (make-sparse-keymap)
      outline-repeat-map (make-sparse-keymap)
      project-dired-prefix-map (make-sparse-keymap)
      meow-grep-prefix-map (make-sparse-keymap)
      treesit-auto-install-grammar 'always)

;; unbind `kill-region' (used for window keymap)
(unbind-key "C-w")
;; unbind `suspend-emacs' (used for search keymap)
(unbind-key "C-z")
;; unbind `undo' (used for completion keymap)
(unbind-key "C-/")
;; unbind `scroll-down-command'
(unbind-key "C-v")
;;(unbind-key "C-\[")
;; unbind `abort-recursive-edit'
(unbind-key "C-\]")

(define-prefix-command 'meow-toggle-prefix-command 'meow-toggle-prefix-map "toggle")
(define-prefix-command 'meow-quit-prefix-command 'meow-quit-prefix-map "quit")
(define-prefix-command 'meow-vc-prefix-command 'meow-vc-prefix-map "vc")
(define-prefix-command 'meow-window-prefix-command 'meow-window-prefix-map "window")
(define-prefix-command 'meow-code-prefix-command 'meow-code-prefix-map "code")
(define-prefix-command 'meow-mark-prefix-command 'meow-mark-prefix-map "mark")
(define-prefix-command 'meow-C-z-prefix-command 'meow-C-z-prefix-map "C-z")
(define-prefix-command 'meow-C-/-prefix-command 'meow-C-/-prefix-map "C-/")
(define-prefix-command 'project-dired-prefix-command 'project-dired-prefix-map "project-dired")
(define-prefix-command 'meow-grep-prefix-command 'meow-grep-prefix-map "grep")
(define-prefix-command 'meow-C-v-prefix-command 'meow-C-v-prefix-map "vc")


(bind-key "C-c t" meow-toggle-prefix-map)
(bind-key "C-c q" meow-quit-prefix-map)
(bind-key "C-c g" meow-vc-prefix-map)
(bind-key "C-w" 'meow-window-prefix-command)
(bind-key "C-c c" meow-code-prefix-map)
(bind-key "C-c m" meow-mark-prefix-map)
(bind-key "C-c z" meow-grep-prefix-map)
(bind-key "C-z" 'meow-C-z-prefix-command)
(bind-key "C-/" 'meow-C-/-prefix-command)
(bind-key "C-x pd" 'project-dired-prefix-command)
(bind-key "C-v" vc-prefix-map)

(setopt indent-tabs-mode nil
        blink-cursor-mode nil)

(setq-default backup-inhibited t
              line-spacing 0.0
              help-char (aref (kbd "M-h") 0)
              create-lockfiles nil
              truncate-lines t
              make-backup-files nil
              backward-delete-char-untabify-method 'hungry
              indicate-buffer-boundaries nil
              cursor-in-non-selected-windows nil
              indicate-empty-lines nil
              treesit-font-lock-level 3
              treesit-sexp-thing 'sexp
              set-mark-command-repeat-pop t
              fill-column 80
              display-fill-column-indicator-character ?│
              global-display-fill-column-indicator-modes '(prog-mode)
              jit-lock-chunk-size 4000
              jit-lock-defer-time nil
              jit-lock-stealth-time 0.5
              tab-width 4
              electric-indent-chars'(?\n ?\^?)
              ;; disable bidirectional text scanning
              bidi-display-reordering t
              bidi-paragraph-direction 'left-to-right
              scroll-step 1
              scroll-conservatively 0
              scroll-up-aggressively 0.0
              scroll-down-aggressively 0.0
              scroll-preserve-screen-position nil
              maximum-scroll-margin 0.25
              scroll-margin 7
              vertical-scroll-bar nil)

(setq user-full-name "Błażej Niewiadomski"
      user-mail-address "blaz.nie@protonmail.com"
      visible-bell nil
      ring-bell-function #'ignore
      display-line-numbers-type 'relative
      display-raw-bytes-as-hex t
      hl-line-sticky-flag nil
      global-hl-line-sticky-flag nil
      global-hl-line-buffers '(not
                               (or (lambda (b) (buffer-local-value 'cursor-face-highlight-mode b))
                                   (lambda (b) (string-match-p "\\` " (buffer-name b))) minibufferp
                                   (major-mode . eat-mode)
                                   (major-mode . dired-sidebar-mode)))
      x-stretch-cursor nil
      echo-keystrokes 0.01
      ;; scrolling
      scroll-error-top-bottom t
      pixel-scroll-precision-use-momentum t
      pixel-scroll-precision-interpolate-mice nil
      fast-but-imprecise-scrolling t
      hscroll-step 1
      hscroll-margin 2
      auto-hscroll-mode t
      scroll-minibuffer-conservatively nil
      lazy-highlight-initial-delay 0.1
      lazy-highlight-no-delay-length 4
      lazy-highlight-buffer t
      completion-ignore-case t
      sentence-end-double-space nil
      read-extended-command-predicate 'command-completion-default-include-p
      use-dialog-box nil
      use-file-dialog nil
      use-short-answers t
      read-answer-short t
      next-screen-context-lines 1
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
      proced-auto-update-interval 3
      visible-cursor nil
      global-mark-ring-max 32
      mark-ring-size 32
      explicit-shell-file-name (+os/per-system! :linux "bash" :win "powershell.exe")
      savehist-autosave-interval nil
      savehist-additional-variables
      '(kill-ring
        register-alist
        mark-ring
        global-mark-ring
        search-ring
        regexp-search-ring
        comint-input-ring
        kmacro-ring)
      ;; kmacro
      kmacro-ring-max 32
      ;; imenu
      imenu-auto-rescan t
      imenu-flatten t
      imenu-use-popup-menu 'on-mouse
      text-scale-mode-step 1.1
      font-lock-maximum-decoration 2
      delete-selection-save-to-register ?r
      register-use-preview t
      register-preview-delay 0.5
      windmove-wrap-around nil
      minibuffer-prompt-properties '(read-only t intangible t cursor-intangible t face minibuffer-prompt)
      enable-recursive-minibuffers t
      completion-preview-minimum-symbol-length 4
      completion-preview-idle-delay 2
      dabbrev-upcase-means-case-search t
      dabbrev-ignored-buffer-modes '(archive-mode image-mode docview-mode pdf-view-mode tags-table-mode csv-mode)
      treesit-enabled-modes t
      uniquify-buffer-name-style 'forward
      ;;windows
      highlight-nonselected-windows nil
      global-text-scale-adjust-resizes-frames nil
      truncate-partial-width-windows nil
      ;;
      kill-do-not-save-duplicates t
      save-interprogram-paste-before-kill t
      ;; undo
      undo-limit (* 2 1000 1000)
      undo-strong-limit (* 4 1000 1000)
      undo-outer-limit (* 24 1000 1000)
      ;; eval expr
      eval-expression-print-length 100
      eval-expression-print-level 5
      ;;recentf
      recentf-max-saved-items 300
      recentf-max-menu-items 15
      ;; tramp
      remote-file-name-inhibit-cache 50
      tramp-verbose 1
      tramp-auto-save-directory (expand-file-name "tramp-autosave/" user-emacs-directory)
      bookmark-save-flag 1
      delete-by-moving-to-trash (not noninteractive)
      remote-file-name-inhibit-delete-by-moving-to-trash t
      find-file-suppress-same-file-warnings t
      find-file-visit-truename t
      ;; auto save
      auto-save-default nil
      auto-save-no-message t
      auto-save-include-big-deletions t
      auto-save-list-file-prefix (expand-file-name "autosave/" user-emacs-directory)
      ;; backup
      backup-directory-alist `(("." . ,(expand-file-name "backup" user-emacs-directory)))
      tramp-backup-directory-alist backup-directory-alist
      backup-by-copying-when-linked t
      backup-by-copying t
      delete-old-versions t
      version-control t
      kept-new-versions 5
      kept-old-versions 5
      ;; editing
      delete-pair-blink-delay 0.03
      comment-multi-line t
      comment-empty-lines t
      ;; disable bidi
      bidi-inhibit-bpa t
      ;; skip fontificaiton during input
      redisplay-skip-fontification-on-input t
      ;; repeat mode
      repeat-exit-timeout 5)

(bind-keys* ("C-M-=" . text-scale-increase)
            ("C-M--" . text-scale-decrease))

(let ((customization-file (expand-file-name "custom.el" user-emacs-directory)))
  (unless (file-exists-p customization-file)
    (write-region "" nil customization-file))
  (setq custom-file customization-file)
  (load custom-file 'noerror))

(ii/defhook! ii/setup-global-modes ()
  "Turn on global modes (after initialization to not fuck up the state)."
  :hook-var after-init-hook
  (global-hl-line-mode 1)
  (save-place-mode 1)
  (recentf-mode 1)
  (column-number-mode 1)
  (line-number-mode -1)
  (delete-selection-mode 1)
  (savehist-mode 1)
  ;; strip text properties before saving the kill ring
  (ii/defhook! ii/savehist-strip-props ()
    "Strip properties from text in kill ring."
    :hook-var savehist-save-hook
    (setq kill-ring
          (mapcar #'substring-no-properties
                  (cl-remove-if-not #'stringp kill-ring))))
  (unless init-file-debug
    (desktop-save-mode 1)
    (desktop-read desktop-dirname)))

;; make desktop save mode ignore some minor modes
(with-eval-after-load 'desktop
  ;; envrc-mode
  (add-to-list 'desktop-minor-mode-table '(envrc-mode nil))
  (add-to-list 'desktop-minor-mode-table '(envrc-global-mode nil)))

;; load up `electric-pair-mode' only when its time for inserting text
(ii/eval-on-first-hook meow-insert-enter-hook "electric-pairs" t (electric-pair-mode 1))

;; add an option to diff current buffer with its file on disk
(add-to-list 'save-some-buffers-action-alist
             (list "d"
                   (lambda (buffer)
                     (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))
(use-package emacs
  :init
  (setq ii/goto-repeat-map (make-sparse-keymap))

  (with-eval-after-load 'polymode
    (advice-add 'display-line-numbers-mode :around #'polymode-inhibit-in-indirect-buffers))

  (defun ii/backward-down-list (arg interactive)
    (interactive "^p\nd")
    (down-list (- arg) interactive))

  (with-eval-after-load 'completion-preview
    (bind-keys
     :map completion-preview-active-mode-map
     ("M-n" . completion-preview-next-candidate)
     ("M-p" . completion-preview-prev-candidate)
     ("TAB" . completion-at-point)
     ("<tab>" . completion-at-point)
     ("M-TAB" . completion-preview-insert)))

  (defun ii/filter-lines (negated)
    (interactive "p")
    (if negated
        (command-execute #'flush-lines)
      (command-execute #'keep-lines)))

  (defun ii/visual-line--setup ()
    (cond
     ((bound-and-true-p visual-line-mode)
      (visual-wrap-prefix-mode 1))
     (t (visual-wrap-prefix-mode -1))))

  (ii/when-idle! 2.0
    (require 'server)
    (when (not (server-running-p))
      (server-mode 1)))
  :config
  ;; setup loading packages on focus loss
  (add-function :after after-focus-change-function
                (defun ii/on-focus-out-load (&rest args)
                  (when (ii/emacs-unfocused-p)
                    (if (not (null ii/load-on-focus-loss-list))
                        (let ((f (pop ii/load-on-focus-loss-list)))
                          (message "Loaded %s" f)
                          (require f))
                      (let ((gc-cons-threshold 80000))
                        (garbage-collect-maybe 1))))))

  (defun ii/reload-path-from-shell (arg)
    (interactive "P")
    (let ((default-directory (if (null arg)
                                 user-emacs-directory
                               (read-directory-name "Reload exec path from:"))))
      (+set-exec-path-from-shell)))

  :bind
  (("<left>" . (lambda () (interactive) (message "No arrows!")))
   ("<right>" . (lambda () (interactive) (message "No arrows!")))
   ("<up>" . (lambda () (interactive) (message "No arrows!")))
   ("<down>" . (lambda () (interactive) (message "No arrows!")))
   ("M-z" . zap-zap-up-to-char)
   ("C-x C-b" . ibuffer)
   ("M-u" . upcase-dwim)
   ("M-l" . downcase-dwim)
   ("M-c" . capitalize-dwim)
   ("C-TAB" . completion-at-point)
   ("C-<tab>" . completion-at-point)
   ("C-c to" . toggle-option)
   ("C-c tde" . toggle-debug-on-error)
   ("C-c tdq" . toggle-debug-on-quit)
   ("C-c tl" . scroll-lock-mode)
   ("C-c ts" . toggle-case-fold-search)
   ("C-c q C-s" . save-buffers-kill-emacs)
   ("C-c q C-a" . kill-emacs)
   ("C-c q C-b" . kill-current-buffer)
   ("C-c q C-r" . restart-emacs)
   ("C-c b C-k" . kill-buffer)
   ("C-c b C-n" . narrow-to-region)
   ("C-c b C-w" . widen)
   ("C-c rp" . ii/reload-path-from-shell)
   ("C-/ C-l" . copy-from-above-command)
   ("C-c sr" . query-replace)
   ("C-c sg" . query-replace-regexp)
   ("C-c sl" . keep-lines)
   ("C-c ks" . kmacro-start-macro)
   ("C-c ke" . kmacro-end-macro)
   ("C-c tA" . artist-mode)

   ;; lines
   ("C-c ld" . kill-matching-lines)
   ("C-c lw" . copy-matching-lines)
   ("C-c ls" . sort-lines)
   ("C-c lf" . flush-lines)
   ("C-c lt" . transpose-lines)
   ("C-c lo" . occur)
   ("C-c lb" . delete-blank-lines)
   ("C-c le" . ensure-empty-lines)
   ("C-c lx" . delete-duplicate-lines)
   ("C-c lh" . highlight-lines-matching-regex)
   ("C-c l." . what-line)
   ("C-c lk" . keep-lines)
   ("C-c lK" . consult-keep-lines)
   ("C-c lg" . goto-line)
   ("C-c lG" . consult-goto-line)
   ("C-c l/" . consult-focus-lines)

   ;;;; windows
   ("C-w C-h" . windmove-left)
   ("C-w C-j" . windmove-down)
   ("C-w C-k" . windmove-up)
   ("C-w C-l" . windmove-right)
   ("C-w H" . windmove-swap-states-left)
   ("C-w J" . windmove-swap-states-down)
   ("C-w K" . windmove-swap-states-up)
   ("C-w L" . windmove-swap-states-right)
   ("C-w h" . windmove-display-left)
   ("C-w j" . windmove-display-down)
   ("C-w k" . windmove-display-up)
   ("C-w l" . windmove-display-right)
   ("C-w C-f" . windmove-display-new-frame)
   ("C-w C-t" . windmove-display-new-tab)
   ("C-w C-d h" . windmove-delete-left)
   ("C-w C-d j" . windmove-delete-down)
   ("C-w C-d k" . windmove-delete-up)
   ("C-w C-d l" . windmove-delete-right)
   ("C-w C-r n" . rotate-windows)
   ("C-w C-r p" . rotate-windows-back)
   ("C-w C-r N" . window-layout-rotate-clockwise)
   ("C-w C-r P" . window-layout-rotate-anticlockwise)
   ("C-c tp" . completion-preview-mode))
  :bind*
  (;; dont use arrows
   ("<left>" . (lambda () (interactive) (message "No arrows!")))
   ("<right>" . (lambda () (interactive) (message "No arrows!")))
   ("<up>" . (lambda () (interactive) (message "No arrows!")))
   ("<down>" . (lambda () (interactive) (message "No arrows!")))
   ("<menu>" . context-menu-open)
   ("M-]" . forward-paragraph)
   ("M-[" . backward-paragraph)
   :map completion-in-region-mode-map
   ("RET" . minibuffer-choose-completion)
   ("M-n" . minibuffer-next-completion)
   ("M-p" . minibuffer-previous-completion)
   :map goto-map
   ("\]p" . forward-paragraph)
   ("\[p" . backward-paragraph)
   ("\]." . forward-sentence)
   ("\[." . backward-sentence)
   ("\[f" . beginning-of-defun)
   ("\]s" . forward-sexp)
   ("\[s" . backward-sexp)
   ("\]l" . forward-list)
   ("\[l" . backward-list)
   ("\]}" . up-list)
   ("\[{" . backward-up-list)
   ("\]/" . down-list)
   ("\[/" . ii/backward-down-list)
   ("\]i" . forward-to-indentation)
   ("\[i" . backward-to-indentation)
   ("g" . beginning-of-buffer)
   ("e" . end-of-buffer)
   ("f" . find-file-at-point)
   ("w" . browse-url-at-point)
   ("j" . next-line)
   ("k" . previous-line)
   ("|" . move-to-column)
   ("l" . move-end-of-line)
   ("h" . move-beginning-of-line)
   ("s" . forward-whitespace)
   ("+" . duplicate-dwim))
  :hook
  ((window-setup-hook server-after-make-frame-hook) . +font--setup)
  (visual-line-mode-hook . ii/visual-line--setup)
  ((help-mode-hook helpful-mode-hook) . visual-line-mode)
  ((gfm-mode-hook prog-mode-hook) . display-line-numbers-mode)
  (after-init-hook . (lambda ()
                       (message (emacs-init-time)))))

(use-package implicit-utils
  :straight `(implicit-utils :type nil
                             :local-repo ,(expand-file-name "config" ii/elisp-path))
  :autoload
  (+utils-whole-buffer-as-string
   +utils-get-region-contents
   +utils--desktop-buffer-predicate)
  :commands
  (+utils/open-random-file-in-dir)
  :bind
  (("M-<backspace>" . backward-kill-word)
   ("C-c f%" . +utils/open-random-file-in-dir)
   ("C-c fC" . +utils/copy-visited-file)
   ("C-c fD" . +utils/delete-visited-file)
   ("C-c fP" . +utils/browse-modules)
   ("C-c fp" . +utils/ripgrep-user-directory)
   ("C-c fy" . +utils/yank-current-file)
   ("C-c fY" . +utils/yank-current-path)
   ("C-c tf" . +utils/consult-set-font-family)
   ("C-c i!" . +utils/insert-shell-command-output)
   :map goto-map
   ("]f" . +utils/forward-defun)
   :repeat-map next-defun-repeat-map
   ("\]" . +utils/forward-defun)
   ("\[" . +utils/backward-defun)))

(use-package implicit-ui
  :straight `(implicit-ui :type nil
                          :build (:not compile) ;; async requires not compiling
                          :local-repo ,(expand-file-name "ui" ii/elisp-path))
  :hook
  (gitignore-mode-hook . ii/gitignore-count-mode))

(use-package implicit-dired
  :straight `(implicit-dired :type nil
                             :local-repo ,(expand-file-name "dired" ii/elisp-path))
  :bind
  (("C-c d*" . ii/fd-rg-dired)
   ("C-c d/" . ii/fd-dired)
   ("C-c dn" . ii/fd-dired-glob)
   :map project-prefix-map
   ("d*" . ii/fd-rg-dired-project)
   ("d/" . ii/fd-dired-project)
   ("dn" . ii/fd-dired-glob-project)))

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
(set-display-table-slot standard-display-table 'truncation (make-glyph-code 32))

(when (featurep 'tty-child-frames)
  (when (fboundp 'tty-tooltip-mode)
    (tty-tooltip-mode 1))
  (set-display-table-slot standard-display-table 'box-vertical (vector ?|))
  (set-display-table-slot standard-display-table 'box-horizontal (vector ?─))
  (set-display-table-slot standard-display-table 'box-down-right (vector ?┌))
  (set-display-table-slot standard-display-table 'box-down-left (vector ?┐))
  (set-display-table-slot standard-display-table 'box-up-right (vector ?└))
  (set-display-table-slot standard-display-table 'box-up-left (vector ?┘))
  (set-display-table-slot standard-display-table 'box-double-vertical (vector ?║))
  (set-display-table-slot standard-display-table 'box-double-horizontal (vector ?═))
  (set-display-table-slot standard-display-table 'box-double-down-right (vector ?╔))
  (set-display-table-slot standard-display-table 'box-double-down-left (vector ?╗))
  (set-display-table-slot standard-display-table 'box-double-up-right (vector ?╚))
  (set-display-table-slot standard-display-table 'box-double-up-left (vector ?╝)))

(setq which-key-popup-type 'side-window
      which-key-preserve-window-configuration nil
      which-key-max-description-length 100
      which-key-side-window-max-width 0.2
      which-key-idle-delay 100000
      which-key-show-early-on-C-h t
      ;;which-key-allow-multiple-replacements t
      which-key-side-window-max-height 0.12
      which-key-idle-secondary-delay 0.05
      which-key-separator " "
      which-key-sort-order 'which-key-key-order-alpha
      which-key-side-window-slot 2
      which-key-max-display-columns 5
      which-key-prefix-prefix "+"
      which-key-compute-remaps t
      which-key-add-column-padding 1
      which-key-show-remaining-keys t
      which-key-min-column-description-width 0)


;; load which key only on first non-insert keypress
(ii/defhook! ii/which-key-setup ()
  "Setup `which-key' on first command thats not a self insert or `M-x'."
  :hook-var pre-command-hook
  :once t
  (when (not (member this-command '(self-insert-command execute-extended-command)))
    (repeat-mode 1)
    (require 'which-key)
    (which-key-mode 1)))


(which-key-add-keymap-based-replacements global-map
  "C-c a" "LLM"
  "C-c b" "Buffer"
  "C-c c" "Compile"
  "C-c d" "Directory"
  "C-c e" "Edit"
  "C-c f" "File"
  "C-c g" "Git"
  "C-c h" "Help"
  "C-c i" "Insert"
  "C-c j" "Jump"
  "C-c k" "Kmacro"
  "C-c l" "Lines"
  "C-c m" "Mark"
  "C-c n" "Notes"
  "C-c o" "Open"
  "C-c p" "Projects"
  "C-c q" "Quit"
  "C-c r" "Run"
  "C-c s" "Search"
  "C-c t" "Toggle"
  "C-c u" "UNUSED"
  "C-c v" "UNUSED"
  "C-c w" "Window"
  "C-c x" "UNUSED"
  "C-c y" "UNUSED"
  "C-c z" "Grep")

(which-key-add-keymap-based-replacements mode-specific-map
  "a" "LLM"
  "b" "Buffer"
  "c" "Compile"
  "d" "Directory"
  "e" "Edit"
  "f" "File"
  "g" "Git"
  "h" "Help"
  "i" "Insert"
  "j" "Jump"
  "k" "Kmacro"
  "l" "Lines"
  "m" "Mark"
  "n" "Notes"
  "o" "Open"
  "p" "Projects"
  "q" "Quit"
  "r" "Run"
  "s" "Search"
  "t" "Toggle"
  "u" "UNUSED"
  "v" "UNUSED"
  "w" "Window"
  "x" "UNUSED"
  "y" "UNUSED"
  "z" "Grep")

(which-key-add-key-based-replacements
  ;; ;; keypad
  ;; "<space> x k" "Kmacro"
  ;; "SPC x k" "Kmacro"
  ;; kmacro map
  "C-x C-k" "Kmacro"
  "C-x C-k C-q" "Cond macro counter"
  "C-x C-k C-r" "Counter to register"
  "C-x C-k C-r a" "Cond counter to register"
  ;; M-g
  "M-g b" "Buffer"
  "M-g \[" "Goto Prev"
  "M-g ]" "Goto Next"
  ;; C-x map
  "C-x 4" "Other Window"
  "C-x 5" "Other Frame"
  "C-x RET" "Input System"
  "C-x p" "Project"
  "C-x r" "Register"
  "C-x n" "Narrow"
  "C-x t" "Tab bar"
  "C-x t^" "Detach bar"
  "C-x a" "Abbrev"
  "C-x ai" "Add Abbrev"
  "C-x w" "Window"
  "C-x w^" "Detach"
  "C-x wf" "Flip"
  "C-x wo" "Rotate"
  "C-x wr" "Rotate Layout"
  "C-x v" "VC"
  "C-x vB" "Since merge base"
  "C-x vb" "Branch"
  "C-x vM" "Merge"
  "C-x vT" "Outstanding"
  "C-x vw" "Working tree"
  "C-x 8" "Insert Char"
  "C-x 8 e" "Emoji"
  "C-x x" "Buffer Content"
  "C-x X" "Edebug"
  "C-x C-a" "Edebug"
  ;; C-c map
  "C-c @" "Outline"
  "C-c a" "LLM"
  "C-c b" "Buffer"
  "C-c b`" "Switch to last buffer"
  "C-c c" "Code"
  "C-c d" "Directory"
  "C-c e" "Edit"
  "C-c et" "Transpose"
  "C-c f" "Files"
  "C-c g" "Git"
  "C-c h" "Help"
  "C-c i" "Insert"
  "C-c j" "Jump"
  "C-c k" "Kmacro"
  "C-c l" "Line"
  "C-c m" "Mark"
  "C-c n" "Notes"
  "C-c o" "Open"
  "C-c p" "Project"
  "C-c q" "Quit"
  "C-c r" "Run"
  "C-c s" "Search"
  "C-c t" "Toggle"
  "C-c td" "Debug"
  "C-c tw" "Window"
  "C-c u" "U"
  "C-c v" "V"
  "C-c w" "Window"
  "C-c x" "X"
  "C-c y" "Y"
  "C-c z" "Z"
  "C-c '" "Normal mode map"
  ;; window map
  "C-w C-d" "Delete"
  "C-w C-r" "Rotate"
  ;; C-v
  "C-v B" "Since merge base"
  "C-v b" "Branch"
  "C-v M" "Merge"
  "C-v T" "Outstanding"
  "C-v w" "Working tree")

(use-package kkp
  :demand
  :config
  (global-kkp-mode 1))

(use-package term-keys
  :straight (term-keys :type git
                       :host github
                       :repo "CyberShadow/term-keys")
  :hook
  (tty-setup-hook . term-keys-mode))

(use-package kitty-graphics
  :straight ( :type git
              :host github
              :repo "cashmeredev/kitty-graphics.el")
  :init
  (setq kitty-gfx-enable-video t)
  (kitty-graphics-setup))

(use-package implicit-meow
  :straight `(implicit-meow :type nil
                            :local-repo ,(expand-file-name "meow" ii/elisp-path))
  ;; :after (meow)
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
  :bind
  (("C-c ei" . ii/meow-increment-number-at-point)
   ("C-c ed" . ii/meow-decrement-number-at-point)
   ("C-c ee" . ii/meow-iedit-mode)
   :map goto-map
   ("C-c" . ii/meow-switch-case)
   ("~" . ii/meow-toggle-case-region)
   ("u" . ii/meow-downcase-dwim)
   ("U" . ii/meow-upcase-dwim)
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
  ;;  :disabled
  :defer t
  :init
  (use-package-autoload-keymap
   'surround-keymap 'surround t)
  :bind
  ( :map surround-keymap
    ("a" . surround-insert)))

;; operating on numbers
(use-package operate-on-number
  :straight ( :type git
              :host github
              :repo "knu/operate-on-number.el")
  :init
  (setq ii/operate-on-number-map (make-sparse-keymap))
  :bind
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

(use-package iedit
  :init
  (defun ii/iedit-with-restriction (beg end)
    (save-mark-and-excursion
      (narrow-to-region beg end)
      (iedit-mode 1)))

  (defun ii/iedit-in-thing (thing)
    (interactive (list (meow-thing-prompt "Iedit inside: ")))
    (when-let* ((bounds (meow--parse-bounds-of-thing-char thing))
                (beg (car bounds))
                (end (cdr bounds)))
      (ii/iedit-with-restriction beg end)))

  (setq iedit-mode-line '(" ie:"
                          (:eval
                           (format #("%d/%d" 0 5 (face font-lock-regexp-face))
                                   iedit-occurrence-index (iedit-counter)))))
  :bind
  ( :map isearch-mode-map
    ("M-d" . iedit-mode-from-isearch)))

(use-package zones)

(use-package meow
  :custom
  (meow-use-keypad-when-execute-kbd t)
  :init
  (advice-add 'meow--select :after (lambda (selection &optional activate backwards)
                                     (message "%S" selection)))

  (setopt meow-use-dynamic-face-color nil
          meow-update-display-in-macro nil
          meow-use-clipboard t
          meow-esc-delay 0
          meow-expand-selection-type 'expand
          meow-pop-or-unpop-to-mark-repeat-unpop t
          meow-mode--set-explicitly nil
          meow-select-on-change nil)

  (defun ii/meow--line-numbers-toggle (&rest args)
    (when (and (bound-and-true-p display-line-numbers-mode) (null meow--beacon-overlays))
      (pcase display-line-numbers-type
        ((or 'relative 'visual)
         (when meow-insert-mode
           (setq-local display-line-numbers-type t)
           (display-line-numbers--turn-on)))
        (_ (when (not meow-insert-mode)
             (setq-local display-line-numbers-type (if (bound-and-true-p visual-line-mode) 'visual 'relative))
             (display-line-numbers--turn-on))))))

  (defun meow-noop ()
    (interactive))

  ;; integration for `xref-edit-mode' in Emacs 31
  (defvar ii/meow--setup-xref-edit nil)

  (defun ii/meow--setup-xref-edit (enable)
    (setq ii/meow--setup-xref-edit enable)
    (if enable
        (progn
          (advice-add 'xref-change-to-xref-edit-mode :after #'meow--switch-to-normal)
          (advice-add 'xref-edit-save-changes :after #'meow--switch-to-motion))
      (advice-remove 'xref-change-to-xref-edit-mode #'meow--switch-to-normal)
      (advice-remove 'xref-edit-save-changes #'meow--switch-to-motion)))

  (with-eval-after-load 'xref
    (ii/meow--setup-xref-edit t))

  (ii/defhook! ii/meow-setup-xref-edit ()
    "Setup automatic meow state switching for `xref-edit-mode'."
    :hook-var meow-global-mode-hook
    (unless (bound-and-true-p meow-global-mode)
      (ii/meow--setup-xref-edit nil)))


  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

    ;;;; ghostel state
    (meow-define-state ghostel
      "meow state for `ghostel'."
      :keymap meow-ghostel-state-map)

    (setq meow-cursor-type-ghostel 'bar)

    (meow-define-keys 'ghostel
      '("C-o" . ghostel-other)
      '("C-g" . ghostel-send-C-g)
      '("M-SPC" . meow-keypad)
      '("C-w" . meow-window-prefix-command))

    ;;;; eat state
    (meow-define-state eat
      "meow state for `eat-mode'."
      :keymap meow-eat-state-map)

    (setq meow-cursor-type-eat 'bar
          meow-cursor-type-insert '(bar . 2))

    (meow-define-keys 'eat
      '("<escape>" . eat-self-input)
      '("M-SPC" . meow-keypad)
      '("C-g" . eat-self-input)
      (cons "C-t" meow-eat-toggle-map))

    (defvar-keymap meow-view-state-keymap
      :doc "Map for `meow-view-mode'."
      :parent meow-motion-state-keymap
      "<remap> <self-insert-command>" #'meow-noop
      "j" #'meow-next
      "k" #'meow-prev
      "z" #'recenter
      "c" #'recenter
      "C-p" #'backward-page
      "C-n" #'forward-page
      "V" #'meow-normal-mode
      "SPC" #'meow-keypad)

    (defvar meow-macro-sequence nil)

    ;;;;macro state (WIP)
    ;; TODO: actually implement this
    (defvar-keymap meow-macro-state-keymap
      :doc "Keymap for `meow-macro-mode'."
      :parent meow-normal-state-keymap
      "<remap> <meow-grab>" #'meow-noop)

    (defvar meow--beacon-macro-enter-key nil
      "TODO")

    (defvar running-macros nil
      "TODO")

    (defvar meow-macro-sequence-size kmacro-ring-max)

    (defvar meow-macro-sequence-ring (make-ring meow-macro-sequence-size))

    (defvar meow-macro-sequence-last-state nil)

    (defun meow-macro (start)
      (interactive "p")
      (if running-macros
          (if meow-macro-mode
              (meow--switch-state 'normal)
            (meow--switch-state 'macro))
        (pcase (meow--current-state)
          ('normal (if defining-kbd-macro
                       (call-interactively #'kmacro-end-macro)
                     (call-interactively #'kmacro-start-macro)))
          ('macro (when defining-kbd-macro
                    (meow-beacon-macro-exit)))
          ('beacon (meow--switch-state 'macro)
                   (call-interactively #'kmacro-start-macro)
                   (setq-local meow--beacon-macro-enter-key last-input-event)
                   (setq meow--beacon-defining-kbd-macro 'quick))
          (_ nil))))

    (defun meow-beacon-macro-exit ()
      (interactive)
      (when defining-kbd-macro
        (end-kbd-macro)
        (meow--beacon-apply-kmacros-from-macro))
      (meow--switch-state 'beacon))

    (defun meow-macro-sequence-save (seq)
      (if (and (ring-p meow-macro-sequence-ring)
               (listp seq))
          (ring-insert meow-macro-sequence-ring seq)
        (user-error "Saving macro sequence failed")))

    (defun meow-macro-insert ()
      (meow-macro-mode -1)
      (meow-insert)
      (when defining-kbd-macro
        (call-interactively #'kmacro-end-macro)
        (add-to-list 'meow-macro-sequence last-kbd-macro))
      (add-to-list meow-macro-sequence 'insert)
      (call-interactively #'kmacro-start-macro))

    ;; (defun meow-beacon-setup-keys ()
    ;;   (when meow-beacon-mode
    ;;     ))
    ;;
    ;; (add-hook 'meow-beacon-mode-hook #'meow-beacon-setup-keys)

    (defun meow-macro-apply-sequence ()
      (meow--beacon-apply-command (lambda ()
                                    (interactive)
                                    (let ((running-macros))
                                      (dolist (macro meow-macro-sequence)
                                        (cond ((vectorp macro)
                                               (let ((last-kbd-macro macro))
                                                 (when (symbolp meow-macro-sequence-last-state)
                                                   (meow--switch-state meow-macro-sequence-last-state))
                                                 (call-interactively #'kmacro-call-macro))
                                               ((symbolp macro)
                                                (setq meow-macro-sequence-last-state macro))
                                               (t nil))))
                                      ;; reset macro state
                                      (setq meow-macro-sequence-last-state nil))))
      (message "macro sequence was %S" meow-macro-sequence)
      (meow-macro-sequence-save meow-macro-sequence)
      (setq meow-macro-sequence nil))

    (defun ii/meow--switch-to-normal ()
      (interactive)
      (if defining-kbd-macro
          (progn
            (message "Defining macro; doing nothing")
            (cancel-kbd-macro-events))
        (meow--switch-to-normal)))

    (defun meow--beacon-apply-kmacros-from-macro ()
      (meow--beacon-apply-command (lambda ()
                                    (interactive)
                                    (let ((inhibit-message t))
                                      (message "macro beacon enter key is %S" meow--beacon-macro-enter-key)
                                      (message "current mode is %S" (meow--current-state)))
                                    (let ((running-macros t))
                                      (meow--execute-kbd-macro ;; execute `meow-macro'
                                       (key-description
                                        (vector meow--beacon-macro-enter-key)))
                                      (call-interactively #'kmacro-call-macro)
                                      (meow-escape-or-normal-modal)))))

    (meow-define-state macro
      "meow state for manipulating selections while recording macros."
      :keymap meow-macro-state-keymap
      (if (and meow-macro-mode
               meow--beacon-defining-kbd-macro)
          (progn (message "Recording macro in \"normal\" mode"))))

    (setq meow-cursor-type-macro 'box)

    (meow-define-keys 'macro
      '("C-z" . meow-beacon-macro-exit)
      '("ESC" . meow-beacon-macro-exit)
      '("<escape>" . meow-beacon-macro-exit))

    (meow-define-keys 'insert
      '("C-z" . ii/meow--switch-to-normal))

    (meow-motion-define-key
     (cons "<escape>" esc-map)
     (cons "ESC" esc-map)
     '("M-SPC" . meow-keypad)
     '("C-w" . meow-window-prefix-command)
     '("j" . meow-next)
     '("k" . meow-prev)
     '("C-s" . +search/buffer))

    (bind-key* "C-x C-k C-s" 'meow-beacon-macro-mode)
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
     '("]" . "M-g ]")
     '("[" . "M-g [")
     (cons "'" meow-normal-state-keymap)
     '("?" . +lookup/documentation)
     '("p" . "C-x p")
     (cons "h" help-map)
     '("w" . "C-w"))

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
     (cons "`" esc-map)
     ;; '(cons "~" ii/text-change-map)
     ;; '("!" . ii/meow-shell-command)
     '("@" . "C-c @")
     (cons "#" ii/operate-on-number-map)
     ;; '("$" . ii/meow-pipe-to-shell-command)
     '("%" . meow-query-replace-regexp)
     ;; '("^")
     ;; '("&")
     ;; '("*")
     '("\(" . "C-x \(")
     '("\)" . "C-x \)")
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
     '("]" . "M-g ]")
     '("[" . "M-g [")
     '("}" . "M-}")
     '("{" . "M-{")
     '("'" . repeat)
     '("\"" . "C-x r")
     '("\\" . mc/mark-next-like-this)
     '("|" . iedit-mode)
     '("/" . +search/buffer)
     '("?" . +lookup/documentation)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("C" . mc/mark-next-like-this)
     '("d" . meow-kill)
     '("D" . meow-kill-append)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("F" . meow-find-expand)
     '("g" .  "M-g")
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
     '("M" . meow-macro)
     '("n" . meow-search)
     '("N" . meow-pop-search)
     '("o" . meow-tree-sitter-node)
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
     '("V" . meow-view-mode)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-save-append)
     '("z" . meow-pop-selection)
     '("Z" . meow-pop-all-selection))

    (defvar-keymap meow-insert-normal-map
      :doc "Keymap for accessing `meow-normal-mode' keybindings in `meow-insert-mode'."
      :keymap meow-normal-state-keymap
      :repeat t)

    (meow-define-keys 'insert
      (cons "C-'" meow-insert-normal-map)
      '("C-z" . meow-macro-mode))

    ;; kbd macro overrides
    (setq meow--kbd-kill-region "S-<delete>"
          meow--kbd-kill-ring-save "C-<insertchar>"
          meow-use-enhanced-selection-effect t
          meow-keypad-leader-dispatch "C-c"
          meow-keypad-ctrl-meta-prefix ?,
          meow-keypad-meta-prefix ?.
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
                                           (kitty-gfx-image-mode . motion)
                                           (calibredb-search-mode . motion)
                                           (fundamental-mode . motion)
                                           (gfm-view-mode . motion)
                                           (ediff-mode . motion)
                                           (calibredb-show-mode . motion)
                                           (emms-playlist-mode . motion)
                                           (nov-mode . motion)
                                           (speed-type-mode . insert)
                                           (shell-mode . insert)
                                           (eat-mode . eat)
                                           (ghostel-mode . ghostel)
                                           (eshell-mode . insert)
                                           (shell-command-mode . motion)
                                           (comint-mode . insert))))

    (setq meow-char-thing-table '((?\( . round) (?\) . round)
                                  (?\[ . square) (?\] . square)
                                  (?\{ . curly) (?\} . curly)
                                  (103 . string) (101 . symbol)
                                  (119 . window) (98 . buffer)
                                  (112 . paragraph) (108 . line)
                                  (118 . visual-line) (100 . defun)
                                  (46 . sentence))
          meow-keypad-start-keys '((?h . ?h) (?x . ?x)))

    (meow-thing-register 'whitespace 'whitespace 'whitespace)
    (add-to-list 'meow-char-thing-table '(32 . whitespace))

    (meow-thing-register 'xml
                         '(pair-regexp ("\<[^\>/]+\>*") ("\<*/[^\</]*\>"))
                         '(pair-regexp ("\<[^\>/]+\>*") ("\<*/[^\</]*\>")))


    (add-to-list 'meow-char-thing-table '(?w . word))
    (add-to-list 'meow-char-thing-table '(?X . xml))
    (add-to-list 'meow-char-thing-table '(?S . sexp))

    (meow-thing-register 'regexp-search
                         'ii/meow--regexp-search-inner
                         'ii/meow--regexp-search-bounds)

    (add-to-list 'meow-char-thing-table '(?/ . regexp-search)))

  (defun +load-meow ()
    "Load `meow'."
    (require 'meow)
    (meow-setup)
    (meow-global-mode 1)
    (add-to-list 'meow-indicator-face-alist '(view . meow-motion-indicator))
    (add-to-list 'meow-indicator-face-alist '(macro . meow-normal-indicator))
    (setq-default meow-replace-state-name-list '((eat . "<T>")
                                                 (ghostel . "<G>")
                                                 (normal . "<N>")
                                                 (motion . "<M>")
                                                 (keypad . "<K>")
                                                 (insert . "<I>")
                                                 (view . "<V>")
                                                 (macro . "<M>")
                                                 (beacon . "<B>"))))
  :bind
  (("M-<backspace>" . meow-backward-kill-symbol)
   ("C-o" . meow-pop-or-unpop-to-mark)
   ("C-`" . meow-pop-to-global-mark)
   ("C-c jf" . find-file-at-point)
   ("C-c twm" . menu-bar-mode)
   ("C-c twt" . tool-bar-mode)
   ("C-c tV" . visual-line-mode)
   ("C-c ic" . insert-char)
   ("C-c ib" . insert-buffer)
   ("C-c b`" . meow-last-buffer)
   ("C-c kl" . meow-kmacro-lines)
   ("C-c km" . meow-kmacro-matches)
   :map goto-map
   ("c" . meow-comment)
   :map emacs-lisp-mode-map
   ("C-c ib" . eval-print-last-sexp)
   :map help-map
   ("l" . load-library))
  :hook
  ;; since archive mode isnt properly detected by meow, i have to do this the hacky way
  (meow-mode-hook . (lambda ()
                      (if (eq major-mode 'archive-mode)
                          (meow-motion-mode 1))))
  ((meow-insert-enter-hook meow-insert-exit-hook) . ii/meow--line-numbers-toggle)
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
 ([remap isearch-forward] . +search/buffer)
 ("C-c s." . +search/rg-thing-at-point)
 ("C-c s?" . +search/affe-grep-thing-at-point)
 ("C-c f." . +search/affe-find-thing-at-point)
 :map goto-map
 ("." . +search/rg-thing-at-point)
 ("?" . +search/affe-grep-thing-at-point)
 ("F" . +search/affe-find-thing-at-point))

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
        fringe-mode (cons 10 7)
        right-margin-width 1)

(use-package debbugs)

(use-package rg
  :init
  (setq rg-align-position-numbers t)
  (defun ii/rg--setup ()
    (setq-local outline-regexp "File.*$"
                outline-heading-end-regexp "\n"))
  :config
  (rg-enable-default-bindings)
  :bind
  (("C-c z'" . rg-dwim)
   ("C-c zp" . rg-project)
   ("C-c zs" . rg-isearch-project)
   ("C-c zS" . rg-isearch-current-dir)
   :map isearch-mode-map
   ("C-/" . rg-isearch-project)
   :map rg-mode-map
   ("i" . wgrep-change-to-wgrep-mode))
  :hook
  (rg-mode-hook . ii/rg--setup))

(with-eval-after-load 'grep
  (defvar-local ii/grep-invis-overlays nil)

  (defun ii/grep-toggle-file-name-visibility ()
    (interactive)
    (save-mark-and-excursion
      (if ii/grep-invis-overlays
          (progn
            (mapc #'delete-overlay ii/grep-invis-overlays)
            (setq ii/grep-invis-overlays nil))
        (goto-char (point-min))
        (let ((prop nil)
              ov num file )
          (while (setq prop (text-property-search-forward 'compilation-message))
            (setq ov (make-overlay (prop-match-beginning prop) (prop-match-end prop)))
            (ignore-errors (save-mark-and-excursion
                             (goto-char (pos-bol))
                             (search-forward-regexp "^\\(\\)\:\\([0-9]+\\)\:")
                             (match-string 1)))

            (overlay-put ov 'display (string-pad num 6 32 t))
            (push ov ii/grep-invis-overlays)))))))

;; emacs >= 31 includes `grep-edit-mode'
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t)
  :bind
  ( :map grep-mode-map
    ("e" . wgrep-change-to-wgrep-mode)))

(use-package ast-grep
  :bind
  (("C-c za" . ast-grep-project)))

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
      (load-theme +base/theme t)))
  :config
  ;; HACK: emacs 31 doesn't allow inheritance cycles, doom-themes still uses them: https://github.com/doomemacs/themes/issues/875
  (setcdr (assoc 'gnus-group-news-low-empty doom-themes-base-faces)
          '(:inherit 'gnus-group-mail-1-empty :weight 'normal))
  :hook
  (after-init-hook . ii/appearance-setup-doom-themes)
  (server-after-make-frame-hook . ii/appearance-setup-doom-themes))

(use-package colorful-mode
  :init
  (setq colorful-use-prefix t
        colorful-only-strings 'only-prog
        colorful-allow-mouse-clicks nil)

  (defvar ii/colorful-highlight-string-modes '(help-mode helpful-mode))

  (ii/defhook! ii/colorful-setup-prefix-use ()
    "Override default `colorful-use-prefix' value for modes in `ii/colorful-highlight-string-modes'."
    :hook-var colorful-mode-hook
    (if (memq major-mode ii/colorful-highlight-string-modes)
        (setq-local colorful-use-prefix nil)))

  :bind
  (("C-c tc" . colorful-mode))
  :hook
  ((css-mode-hook
    css-ts-mode-hook
    web-mode-hook
    prog-mode-hook
    help-mode-hook
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
            ("XXXX*" . compilation-error))))
  :hook ((prog-mode-hook org-mode-hook markdown-mode-hook) . hl-todo-mode)
  :bind
  (("C-c st" . hl-todo-rgrep)
   :map goto-map
   ("]t" . hl-todo-next)
   ("[t" . hl-todo-previous)
   :map search-map
   ("t" . hl-todo-occur)))

(defun ii/doom-theme-set-faces (theme)
  "Setup faces for doom THEME"
  (let ((theme (symbol-name theme)))
    (custom-set-faces
     `(region ((t (:extend nil))))
     `(eglot-inlay-hint-face ((t (:slant italic :inherit font-lock-comment-face)))))
    (if (string-prefix-p "doom" theme)
        (let* ((bg (doom-color 'bg))
               (bg-alt (doom-color 'bg-alt))
               (fg-alt (doom-color 'fg-alt))
               (base0 (doom-color 'base0))
               (base1 (doom-color 'base1))
               (base2 (doom-color 'base2))
               (base3 (doom-color 'base3))
               (base4 (doom-color 'base4))
               (selection (doom-color 'selection))
               (base4 (doom-color 'base4)))
          (progn (message "Loading custom faces")
                 (custom-set-faces
                  `(secondary-selection (( t (:background ,bg :extend nil))))
                  `(cua-rectangle ((t (:background ,selection))))
                  `(minibuffer-nonselected (( t (:background ,bg-alt :foreground ,fg-alt :extend nil))))
                  `(whitespace-space (( t (:foreground ,base3))))
                  `(meow-beacon-fake-selection ((t (:background ,selection :foreground ,(doom-color 'fg)))))
                  `(meow-search-highlight ((t (:background ,selection))))
                  `(iedit-occurrence (( t (:background ,base3 :foreground ,fg-alt :weight bold :inverse-video nil))))
                  `(corfu-popupinfo ((t (:background ,base0))))
                  `(vertico-group-separator (( t (:background ,bg :foreground ,fg-alt :strike-through t))))
                  `(vertico-group-title (( t (:background ,bg :foreground ,fg-alt))))
                  `(wgrep-face (( t (:background ,selection))))
                  `(embark-selected (( t (:background ,selection :foreground unspecified))))
                  `(which-key-posframe-border (( t (:background ,base4))))
                  `(rg-file-tag-face (( t (:foreground ,bg-alt :extend t))))
                  `(match (( t (:background ,bg-alt))))
                  `(colorful-base ((t (:box nil))))
                  `(eldoc-highlight-function-argument ((t (:inverse-video t))))
                  `(eldoc-box-body (( t (:background ,base1))))
                  `(mode-line ((t (:box (:line-width -1 :color ,base4 :style flat-button))))))))
      (custom-reset-faces
       `(secondary-selection ,theme)
       `(minibuffer-nonselected ,theme)
       `(whitespace-space ,theme)
       `(iedit-occurrence ,theme)
       `(vertico-group-separator ,theme)
       `(vertico-group-title ,theme)
       `(wgrep-face ,theme)
       `(embark-selected ,theme)
       `(which-key-posframe-border ,theme)
       `(rg-file-tag-face ,theme)))))

(defun ii/doom-theme-faces-ensure-last ()
  "Ensure that `ii/doom-theme-set-faces' is always the last function in \
`enable-theme-functions'."
  (when (member 'ii/doom-theme-set-faces enable-theme-functions)
    (remove-hook 'enable-theme-functions 'ii/doom-theme-set-faces))
  (add-hook 'enable-theme-functions
            #'ii/doom-theme-set-faces
            99))

(add-hook 'enable-theme-functions #'ii/doom-theme-set-faces 99)

(add-hook 'meow-global-mode-hook #'ii/doom-theme-faces-ensure-last 99)

(use-package doom-gruber-darker-theme
  :straight (doom-gruber-darker-theme :type git
                                      :host github
                                      :repo "implicit-image/doom-gruber-darker-theme"))

;;;; Window Management
(use-package implicit-windows
  :straight `(implicit-windows :type nil
                               :local-repo ,(expand-file-name "windows" ii/elisp-path))
  :bind
  (("C-w C-`" . ii/windows-toggle-minibuffer-focus)
   ("C-w `" . ii/windows-quit-current-minibuffer)
   ("C-w \m" . +windows/toggle-maximize-window)
   ("C-w C-n" . ii/no-window-prefix)
   ;; display action prefixes
   ("C-w C-p C-v" . +windows/below-selected-prefix)
   ("C-w C-p C-b" . +windows/bottom-window-prefix)
   ("C-w C-p C-h" . +windows/left-vsplit-prefix)
   ("C-w C-p C-j". +windows/below-hsplit-prefix)
   ("C-w C-p C-k" . +windows/above-hsplit-prefix)
   ("C-w C-p C-l" . +windows/right-vsplit-prefix)
   ("C-w C-p h" . +windows/left-side-window-prefix)
   ("C-w C-p j" . +windows/bottom-side-windows-prefix)
   ("C-w C-p k" . +windows/top-side-window-prefix)
   ("C-w C-p l" . +windows/right-side-window-prefix)
   ("C-w tM" . +windows/toggle-modeline)))

(use-package window
  :straight nil
  :init
  (setq window-resize-pixelwise nil
        resize-mini-windows 'grow-only
        switch-to-buffer-in-dedicated-window nil
        switch-to-buffer-obey-display-actions nil
        switch-to-buffer-preserve-window-point t
        display-buffer-alist
        '( ;; normal window without a modeline
          ((or . ((derived-mode . calibredb-search-mode)
                  (derived-mode . calibredb-edit-annotation-mode)
                  (derived-mode . calibredb-show-mode)))
           (display-buffer-same-window))
          ;; register preview window
          ((or . ("\*Register Preview\*"
                  " \*Register Preview\*"
                  " \*Register Preview\* "))
           (display-buffer-below-selected)
           (window-parameters . ((mode-line-format . none))))
          ((or . ((derived-mode . vc-git-region-history-mode)
                  (derived-mode . vc-hg-region-history-mode)))
           (display-buffer-use-some-window)
           (post-command-select-window . t)
           (inhibit-same-window . t))
          ((or . ((derived-mode . ibuffer-mode)
                  "\*Ibuffer\*"))
           (display-buffer-reuse-mode-window display-buffer-use-some-window))
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
          ;; embark export
          ((and . ("\*Embark Export:*"
                   (or . ((derived-mode . grep-mode)
                          (derived-mode . occur-mode)))))
           (display-buffer-reuse-mode-window display-buffer-pop-up-window display-buffer-use-some-window))
          ;; bottom side window
          ((or . ((derived-mode . rg-mode)
                  (derived-mode . grep-mode)
                  (derived-mode . xref--xref-buffer-mode)))
           (display-buffer-reuse-mode-window display-buffer-use-some-window)
           ;; (dedicated . t)
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
                  "\*Agenda Commands\*"
                  "\*tide-documentation\*"
                  ;; "\*eldoc\*"
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
           (window-height . 0.3))
          ;; interactive bottom buffers
          ((or . ((derived-mode . flycheck-error-list-mode)
                  (derived-mode . comint-mode)
                  (derived-mode . compilation-mode)
                  (derived-mode . ghostel-compile-view-mode)
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

  :bind
  (("C-c twd" . toggle-window-dedicated)
   ("C-c bp" . previous-buffer)
   ("C-c bn" . next-buffer)
   ("C-w C-o" . other-window-prefix)
   ("C-w C-s" . same-window-prefix)
   ("C-w q" . delete-window)
   ("C-w Q" . kill-buffer-and-window)
   ("C-w v" . split-window-horizontally)
   ("C-w s" . split-window-vertically)
   ("C-w =" . balance-windows)
   ("C-w o" . delete-other-windows)
   ("C-w -" . shrink-window)
   ("C-w +" . enlarge-window)
   ("C-w M-\-" . shrink-window-horizontally)
   ("C-w M-\+" . enlarge-window-horizontally)
   ("C-w w" . other-window)
   ("C-w C-w" . other-window)
   :map goto-map
   ("b[" . previous-buffer)
   ("b]" . next-buffer)
   ("]b" . next-buffer)
   ("[b" . previous-buffer)))

;;;; Modeline

(setq mode-line-right-align-edge 'right-margin
      mode-line-position-column-format '(" %c"))

(add-hook 'minibuffer-mode-hook 'visual-line-mode)

(defvar-local ii/project-name nil
  "TODO")

(defvar-local ii/project-root nil
  "Root of the project the current buffer belongs to,")

(defvar-local ii/breadcrumbs--file-path nil
  "File path for the header line.")

(defun ii/mode-line--get-project-name (project)
  "Get name of PROJECT while handling the cases of non-file buffers."
  (when project
    (let ((bf (buffer-name))
          (pname (project-name project)))
      (if (string-equal pname bf)
          ""
        pname))))

(defun ii/breadcrumbs--should-show-p ()
  (and buffer-file-name
       (not header-line-format)
       (not (memq major-mode '(dired-mode wdired-mode)))))

(defun ii/mode-line-update-project (&rest _args)
  "Update mode line project display."
  (when (ii/breadcrumbs--should-show-p)
    (let* ((p (project-current nil))
           (pname (ii/mode-line--get-project-name p))
           (root (if p
                     (project-root p)
                   (let ((home (expand-file-name "~" ".")))
                     (if (file-in-directory-p default-directory home)
                         (expand-file-name "~" "/")
                       default-directory))))
           (pname (if (and pname (not (string-empty-p pname)))
                      pname
                    (if (length< root 40)
                        root
                      (concat (seq-take root 15) "..." (seq-drop root (- (length root) 15))))))
           (file (file-relative-name buffer-file-name root)))
      (setq-local header-line-format `(,@(if-let* ((remote (file-remote-p buffer-file-name)))
                                             (list remote ii/breadcrumbs--separator)
                                           (list (user-login-name) "@" (system-name) ii/breadcrumbs--separator))
                                       (:propertize ,pname face success)
                                       ,@(when file `(ii/breadcrumbs--separator ,file)))))))
(with-eval-after-load 'polymode
  (advice-add 'ii/mode-line-update-project :around #'polymode-inhibit-during-initialization))

(ii/defhook! ii/project-headerline-setup ()
  "Setup hooks for updating headerline breadcrumbs."
  :hook-var after-init-hook
  :once t
  (add-hook 'window-buffer-change-functions 'ii/mode-line-update-project)
  (add-hook 'after-set-visited-file-name-hook 'ii/mode-line-update-project))

(defvar ii/breadcrumbs--separator " > "
  "Separator for breadcrumbs.")

(ii/defhook! ii/reset-modeline ()
  "Setup proper modeline after initialization."
  :hook-var after-init-hook
  (setq-default header-line-format nil
                mode-line-format
                '(" "
                  (:eval (propertize "@"
                                     'face
                                     (alist-get meow--current-state meow-indicator-face-alist)))
                  (iedit-mode iedit-mode-line)
                  "%n "
                  "%z%*%@ "
                  "%["
                  mode-line-buffer-identification
                  "%] "
                  mode-line-position
                  " %I"
                  mode-line-format-right-align
                  mode-line-misc-info
                  (text-scale-mode text-scale-mode-lighter)
                  mode-line-process
                  (flymake-mode flymake-mode-line-counters)
                  (vc-mode vc-mode)
                  " "
                  mode-name)))

(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      ;; for normal completion mechanism to not suck
      completion-show-help nil
      completion-show-inline-help t
      completion-auto-select nil
      completion-cycling t
      completions-format 'vertical
      completions-max-height 10
      completions-header-format nil
      completions-detailed t
      completions-sort nil)

(add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)

(use-package marginalia
  :custom
  (marginalia-align 'center)
  (marginalia-align-offset 10)
  :init
  (add-hook 'after-init-hook #'marginalia-mode))

(use-package implicit-vertico
  :straight `(implicit-vertico :type nil
                               :local-repo ,(expand-file-name "vertico" ii/elisp-path)))

(use-package vertico
  :init
  (setopt vertico-count 12
          vertico-resize nil
          vertico-cycle t
          vertico-preselect 'first
          vertico-scroll-margin 5
          vertico-grid-max-columns 7
          vertico-flat-format `(:multiple #("{ %s }" 0 1 (face minibuffer-prompt) 3 4 (face minibuffer-prompt))
                                          :single
                                          #("[ %s ]" 0 1 (face minibuffer-prompt) 1 3 (face success) 3 4
                                            (face minibuffer-prompt))
                                          :prompt
                                          #("( %s )" 0 1 (face minibuffer-prompt) 3 4 (face minibuffer-prompt))
                                          :separator #(" | " 0 3 (face minibuffer-prompt)) :ellipsis
                                          #("..." 0 1 (face minibuffer-prompt)) :no-match "[No match]")
          vertico-flat-max-lines 2
          vertico-buffer-display-action '((ii/vertico--buffer-mode-display-buffer)
                                          (reusable-frames . nil)
                                          (popup-frames . nil)
                                          (inhibit-switch-frame . t)))

  :config
  (setopt vertico-multiform-categories
          '((embark-keybinding grid)
            (consult-xref buffer)
            (ast-grep buffer)
            (buffer buffer))
          vertico-multiform-commands
          '((consult-grep buffer)
            (consult-ripgrep buffer)
            (consult-find buffer)
            (consult-fd buffer)
            (consult-info buffer)

            (+utils/browse-modules buffer)
            (consult-outline buffer)
            (consult-buffer buffer)
            (project-find-file buffer)
            (affe-grep buffer)
            (affe-find buffer)
            (consult-line buffer)
            (consult-line-multi buffer)))
  (vertico-multiform-mode)
  :hook
  (marginalia-mode-hook . vertico-mode)
  (minibuffer-setup-hook . vertico-repeat-save)
  :bind*
  (("C-M-`" . vertico-suspend)
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

(use-package embark-consult
  :after (embark)
  :config
  ;; make sure no embark command asks for confirmation
  (dolist (entry embark-pre-action-hooks)
    (delete 'embark--confirm entry))


  (defun ii/embark-consult-export-grep (lines)
    (embark-consult--export-grep
     :header "Exported grep results:\n\n"
     :lines lines
     :insert
     (lambda (lines)
       (save-mark-and-excursion
         (let ((last-file "")
               (inhibit-read-only t)
               file line-num)
           (dolist (line lines)
             (setq file (get-text-property 0 'consult--prefix-group line)
                   line-num (progn (string-match "^.+\:\\([0-9]+(\\).*$" line)
                                   (match-string 1)))
             (unless (string-equal file last-file)
               (let* ((p (point))
                      (ov (make-overlay p p nil nil nil)))
                 (overlay-put ov 'face 'flymake-note-echo-at-eol)
                 (insert (propertize (concat (propertize file 'face 'flymake-note-echo-at-eol) "\n") 'read-only t))
                 (move-overlay ov p (1- (point)))))
             (let ((p (point)))
               (insert line "\n")
               (save-mark-and-excursion
                 (goto-char p)
                 (let ((ov (make-overlay p (1+ (next-single-property-change p 'consult--prefix-group)) nil nil t)))
                   (overlay-put ov 'invisible t)
                   (overlay-put ov 'after-string (concat line-num)))))
             (setq last-file file))))
       (let ((count 0) prop)
         (while (setq prop (text-property-search-forward
                            'face 'consult-highlight-match t))
           (setq count (+ count 1))
           (put-text-property (prop-match-beginning prop)
                              (prop-match-end prop)
                              'font-lock-face
                              'match))
         count))
     :footer #'ignore))

  (advice-add 'embark-consult-export-grep :override #'ii/embark-consult-export-grep)
  :bind
  ( :map embark-consult-search-map
    ([remap consult-find] . consult-fd)))

(use-package embark
  :custom
  (embark-mixed-indicator-delay nil)
  (embark-quit-after-action t)
  (embark-help-key "C-M-h")
  :init
  (advice-add 'embark--confirm :override (lambda (&rest args)
                                           nil))

  (defun ii/embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) " " "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(ii/embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)
        embark-confirm-act-all nil)

  (defun ii/embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'ii/embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'ii/embark-hide-which-key-indicator)
  :bind*
  (("M-." . embark-dwim)
   ("M-'" . embark-act)
   ("C-c m." . embark-select)
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

(use-package consult-eglot
  :init
  (setq consult-eglot-sort-results nil)
  :bind
  ( :map eglot-mode-map
    ("C-c cs" . consult-eglot-symbols)))

(use-package consult
  :autoload
  (consult--read consult--fast-abbreviate-file-name)
  :custom
  (xref-show-xrefs-function 'consult-xref)
  :init
  (setq consult-find-args (+os/per-system! :wsl  (format "%s . -not ( -path */.[A-Za-z]* -prune )"
                                                         find-program)
                                           :linux (format "%s . -not ( -path */.[A-Za-z]* -prune )"
                                                          find-program)
                                           :win (format "%s . -not ( -path */.[A-Za-z]* -prune )"
                                                        find-program))
        consult-narrow-key "C-c C-n"
        consult-preview-partial-size (* 5 1000 1000)
        consult-preview-partial-chunk (* 5 1000 1000)
        consult-async-min-input 1
        consult-async-refresh-delay 0.1
        consult-async-input-debounce 0.2
        consult-async-input-throttle 0.2
        consult-register-prefix "")

  ;; setup preview for `find-file' and `project-find-file' commands
  (with-eval-after-load 'vertico

    (defun ii/consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
      (interactive)
      (let ((default-directory (or dir default-directory))
            (minibuffer-completing-file-name t))
        (consult--read #'read-file-name-internal
                       :state (consult--file-preview)
                       :prompt prompt
                       :require-match mustmatch
                       :predicate pred)))

    (setq read-file-name-function #'ii/consult-find-file-with-preview)

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
                       :predicate pred)))

    (setq project-read-file-name-function #'ii/consult-project-find-file-with-preview))

  :config
  (defvar ii/consult-preview-excluded-modes nil)
  (setq ii/consult-preview-excluded-modes '(image-mode pdf-view-mode dired-sidebar-mode helpful-mode help-mode))

  ;; local var popup interferes with scrolling through previews
  (setf (alist-get 'enable-local-variables consult-preview-variables) :safe)
  ;; dired-sidebar buffer can mess up display if previewed
  (setq consult-preview-excluded-buffers (lambda (buffer &rest args)
                                           (memq (buffer-local-value 'major-mode buffer)
                                                 ii/consult-preview-excluded-modes)))

  (when (and (eq system-type 'windows-nt) (executable-find "es"))
    (defcustom consult-everything-args
      "es -r"
      "Command line arguments for everything, see `consult-everything'.

The default value is \"es -r\", which only works if you place the command line version of Everything (es.exe) in your PATH."
      :type 'string)

    (defun consult--everything-builder (input)
      "Build command line from INPUT."
      (pcase-let ((`(,arg . ,opts) (consult--command-split input)))
        (unless (string-blank-p arg)
          (cons (append (consult--build-args consult-everything-args)
                        (consult--split-escaped arg) opts)
                (cdr (consult--default-regexp-compiler input 'basic t))))))

    (defun ii/consult-everything (&optional initial)
      "Search with `everything' for files matching input regexp given INITIAL input."
      (interactive)
      (find-file (consult--find "Everything: " #'consult--everything-builder initial)))

    (consult-customize
     ii/consult-everything
     :state (consult--file-preview))

    (bind-key* (kbd "C-c ze") #'ii/consult-everything))

  ;; disable previews for compilation errors
  (consult-customize
   consult-compile-error
   :preview-key nil
   consult-find
   project-find-file
   +utils/browse-modules
   consult-fd
   :state (consult--file-preview))

  (add-to-list 'consult-preview-excluded-files "\.gpg")
  (add-to-list 'consult-preview-excluded-files "authinfo\.gpg")
  (add-to-list 'consult-preview-allowed-hooks 'window-buffer-change-functions)
  :bind*
  (("C-c f/" . consult-fd)
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
   ("C-c se" . consult-flymake)
   ("C-c sc" . consult-compile-error)
   ("C-c z/" . consult-ripgrep)
   ("C-c tt" . consult-theme)
   ("C-c kk" . consult-kmacro)
   ("C-c ih" . consult-history)
   ("C-c iy" . consult-yank-pop)
   ("C-c iY" . consult-yank-replace)
   ("C-c ik" . consult-yank-from-kill-ring)
   ("C-c ss" . consult-isearch-history)
   ;; bind also in register map
   ("C-x rr" . consult-register-store)
   ("C-x r\"" . consult-register-load)
   ("C-x r\'" . consult-register-store)
   ("C-x rj" . consult-register)
   ("C-c so" . consult-outline)
   :map help-map
   ("C-i" . consult-info)
   :map project-prefix-map
   ("b" . consult-project-buffer)
   :map goto-map
   ("m" . consult-mark)
   :map vertico-map
   ("C-c C-h" . consult-history)))

(use-package websocket)

(use-package affe
  :if (eq system-type 'gnu/linux)
  :init
  (setq affe-find-command "rg --files --color=never "
        affe-grep-command "rg --null --color=never --max-columns=1000 --no-heading --line-number -v ^$")
  :config
  (require 'orderless)
  (defun affe-orderless-regexp-compiler (input _type _ignorecase)
    (setq input (cdr (orderless-compile input)))
    (cons input (apply-partially #'orderless--highlight input t)))

  (setq affe-regexp-compiler #'affe-orderless-regexp-compiler
        affe-count 1000)

  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep
                     :preview-key '(:debounce 0.05 any)
                     affe-find
                     ii/browse-agenda-files
                     :state (consult--file-state)
                     :preview-key '(:debounce 0.05 any))
  :bind
  (("C-c z?" . affe-grep)
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

(with-eval-after-load 'eldoc
  (remove-hook 'pre-command-hook 'eldoc-pre-command-refresh-echo-area))

(use-package eldoc-box
  :init
  (defun ii/eldoc-box--position-function (width height)
    (let* ((pos (eldoc-box--default-at-point-position-function width height))
           (x (car pos))
           (y (cdr pos)))
      (cons x y)))

  (setq eldoc-box-clear-with-C-g t
        eldoc-box-cleanup-interval 0.1
        eldoc-box-max-pixel-width (byte-compile-lambda
                                   '(lambda ()
                                      (if (display-graphic-p) 800 70)))
        eldoc-box-max-pixel-height (byte-compile-lambda
                                    '(lambda ()
                                       (if (display-graphic-p) 800 30)))
        eldoc-box-position-function 'eldoc-box--default-upper-corner-position-function
        eldoc-box-doc-separator "\n-------------------------------\n"
        eldoc-box-only-multi-line nil
        eldoc-box-fringe-use-same-bg nil)

  (defun ii/eldoc-box--setup ()
    (if (bound-and-true-p eldoc-box-hover-mode)
        (setq-local eldoc-idle-delay 0.1
                    eldoc-documentation-strategy 'eldoc-documentation-compose)))

  ;;; Makes working with child frames in gui and tty clients at once work correctly
  (defvar ii/last-frame-was-tty (and (not noninteractive) (display-graphic-p (selected-frame))))

  (defvar ii/temp-child-frame-var-list '(corfu--frame corfu-popupinfo-frame eldoc-box--frame))

  (defun ii/child-frame-p (frame)
    (and (framep frame)
         (frame-live-p frame)
         (frame-parent frame)))

  (defun ii/reset-child-frames (&rest _)
    "Delete currently existing child frames if the terminal type
 has changed from terminal to X or other way around."
    (interactive)
    (when (not (ii/child-frame-p (selected-frame)))
      (let ((inhibit-message t))
        (let ((curr (not (display-graphic-p (frame-root-frame (selected-frame))))))
          (unless (eq curr ii/last-frame-was-tty)
            (message "resetting child frames")
            (mapc (lambda (frame-symbol)
                    (when-let* ((frame (and (boundp frame-symbol)
                                            (symbol-value frame-symbol)))
                                (ii/child-frame-p frame))
                      (delete-frame frame)
                      (set frame-symbol nil)))
                  ii/temp-child-frame-var-list))
          (setq ii/last-frame-was-tty curr)))))

  (add-function :after
                after-focus-change-function
                #'ii/reset-child-frames)

  (add-hook 'server-after-make-frame-hook #'ii/reset-child-frames)
  :config
  (setf (alist-get 'font eldoc-box-frame-parameters) +base/font-spec
        (alist-get 'border-width eldoc-box-frame-parameters) 0
        (alist-get 'outer-border-width eldoc-box-frame-parameters) 0
        (alist-get 'internal-border-width eldoc-box-frame-parameters) 0)

  (defun ii/eldoc-box-toggle (arg)
    "Toggle what position function `eldoc-box' is using."
    (interactive "p")
    (message "prefix is %S" arg)
    (cond ((eq (prefix-numeric-value arg) 1) (eldoc-box-hover-mode (if (bound-and-true-p eldoc-box-hover-mode) -1 1)))
          (t
           (when eldoc-box-hover-mode
             (setq eldoc-box-position-function
                   (if (eq eldoc-box-position-function 'eldoc-box--default-at-point-position-function)
                       'eldoc-box--default-upper-corner-position-function
                     'eldoc-box--default-at-point-position-function))))))

  :bind
  (("C-c te" . ii/eldoc-box-toggle)
   ("C-c ck" . eldoc-box-help-at-point))
  :hook
  ;; (meow-switch-state-hook . ii/eldoc-box-toggle)
  (eldoc-box-hover-mode-hook . ii/eldoc-box--setup))

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
                    :regex "\\b(struct +[A-Za-z0-9\_]+|int|void|float|object|itemproperty|effect|talent|location|command|action|cassowary|event|json|sqlquery|vector|string)[ \t]+\\s*JJJ *([ \t]*([^{]*))[ \t\n]*{"
                    :tests ("void test() {" "struct ps_struct test(struct ps_effect e, int num)")
                    :not ("void test();" "struct test {" "struct test\n{" "")))
    (add-to-list 'dumb-jump-find-rules
                 '( :type "type" :supports ("rg" "git-grep") :language "nwscript"
                    :regex "\\b(struct \\s*JJJ[\n\t {]*"
                    :tests ("struct test {" "struct test {" "struct test \n{" "struct test \n{")
                    :not ("struct ps_effect test()" "struct test;")))
    (add-to-list 'dumb-jump-find-rules
                 '( :type "variable" :supports ("rg" "git-grep") :language "nwscript"
                    :regexp "\\b(const[ \t]+(int|float|string)[ \t]+)\\s*JJJ[ \t]+=.*"
                    :tests ("const int PS_VAR = 234;" "const string ps_str = \"fsfs\"")
                    :not ("int test = 213213;" "int test" "const vector test = [1, 2, 4];")))
    (add-to-list 'dumb-jump-find-rules
                 '( :type "variable" :supports ("rg" "git-grep") :language "nwscript"
                    :regexp "\\b(struct +[A-Za-z0-9\_]+|int|void|float|object|itemproperty|effect|talent|location|command|action|cassowary|event|json|sqlquery|vector|string)\\s*JJJ.*;"
                    :tests ("struct ps_effect test;" "int test = 423423432;")
                    :not ("test = 23423;" "ps_effect test = FUNC();")))
    (add-to-list 'dumb-jump-language-comments '(:comment "//" :language "nwscript") )
    (add-to-list 'dumb-jump-language-file-exts '(:language "nwscript" :ext "nss" :agtype nil :rgtype nil)))
  :hook
  (xref-backend-functions . dumb-jump-xref-activate))

(setq xref-prompt-for-identifier nil
      xref-search-program 'ripgrep)

(ii/bind-keys "xref"
  ("C-c jr" . xref-find-references)
  ("C-c jd" . xref-find-definitions)
  :map goto-map
  ("r" . xref-find-references)
  ("R" . xref-find-references-and-replace)
  ("d" . xref-find-definitions)
  ("i" . xref-find-implementations)
  ("D" . xref-find-declaration)
  ("y" . xref-find-type-definition)
  :repeat-map xref-repeat-map
  ("," . xref-go-back)
  ("C-," . xref-go-forward))

(setq help-enable-completion-autoload nil
      help-enable-autoload nil
      help-enable-symbol-autoload nil)

(with-eval-after-load 'help
  (defun ii/help--setup ()
    "Setup local lookup function."
    (setq-local +lookup-documentation-function 'helpful-at-point))

  (add-hook 'help-mode-hook 'ii/help--setup)

  (bind-keys*
   :map help-mode-map
   ("?" . +lookup/documentation)))

(ii/bind-keys "help-fns"
  :map help-map
  ("b" . describe-bindings)
  ("F" . describe-face)
  ("m" . describe-keymap)
  ("t" . describe-text-properties)
  ("\\" . describe-input-method)
  ("p" . describe-package)
  ("T" . describe-theme)
  ("M" . describe-mode)
  ("i" . info-lookup-symbol))

(ii/bind-keys "descr-text"
  :map help-map
  ("c" . describe-char))

(ii/bind-keys "woman"
  :map help-map
  ("w" . woman))

(use-package helpful
  :config
  (add-hook 'helpful-mode-hook 'ii/help--setup)

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
   ("C-!" . helpful-macro)))

;;;; Formatting
(setopt editorconfig-exclude-modes '(lisp-interaction-mode))

(ii/eval-on-first-hook find-file-hook "editorconfig" t (editorconfig-mode 1))

(bind-keys
 ("C-c ea" . align)
 ("C-c eA" . align-regexp))

(use-package apheleia
  :defer t
  :init
  (ii/eval-on-first-hook find-file-hook "apheleia" t (apheleia-global-mode 1)))

(use-package undo-fu-session
  :config
  (setopt undo-fu-session-incompatible-major-modes '(authinfo-mode)
          undo-fu-session-incompatible-files '("\.zshrc" ))
  :hook
  (after-init-hook . undo-fu-session-global-mode))

(use-package vundo
  :custom
  (vundo-window-max-height 12)
  :bind
  (("C-x u" . vundo)
   :map vundo-mode-map
   ("r" . vundo-forward)
   ("q" . vundo-confirm)
   ("u" . vundo-backward)
   ("C-/" . vundo-backward)
   ("C-?" . vundo-forward)))

(setq show-paren-style 'parenthesis
      show-paren-delay 0.15
      show-paren-context-when-offscreen t
      show-paren-when-point-inside-paren t
      show-paren-when-point-in-periphery t)

(add-hook 'prog-mode-hook 'show-paren-local-mode)

(use-package window-stool
  :straight (window-stool :type git
                          :host github
                          :repo "JasZhe/window-stool")
  :autoload
  (window-stool-single-overlay)
  :init
  (setopt window-stool-n-from-top 1
          window-stool-n-from-bottom 2)

  (defvar-local ii/window-stool-was-active nil)

  (defmacro ii/window-stool--with-clear-buffer (buffer &rest body)
    "Evaluate BODY in BUFFER without `window-stool' overlay being present."
    (declare (indent defun))
    `(let* ((window (get-buffer-window ,buffer))
            (ov (alist-get window window-stool-overlays)))
       (when ov (delete-overlay ov))
       ,@body
       (window-stool-single-overlay window (window-start window))))

  ;; dont do window stool overlays while in inner polymode buffers.
  (with-eval-after-load 'polymode
    (advice-add 'window-stool-mode :around #'polymode-inhibit-in-indirect-buffers))

  (defun ii/window-stool-show-context ()
    "Show context before point."
    (interactive)
    (require 'window-stool)
    (setq-local window-stool-mode t)
    (window-stool-single-overlay (selected-window) (window-start))
    (add-hook 'post-command-hook #'ii/window-stool--reset t t)
    (setq-local window-stool-mode nil))

  (defun ii/window-stool--reset (&rest args)
    (unless (eq real-this-command 'ii/window-stool-show-context)
      (mapc (lambda (pair)
              (let ((win (car pair))
                    (ov (cdr pair)))
                (when (and (windowp win) (overlayp ov))
                  (delete-overlay ov))))
            window-stool-overlays)
      (remove-hook 'post-command-hook #'ii/window-stool--reset t)))

  (defun ii/window-stool-mode (&optional arg)
    (interactive)
    (if (< (* 1000 1000 1000) buffer-saved-size)
        (window-stool-mode arg)
      (message "This file is too large for `window-stool-mode'")))

  (defun ii/window-stool-hide-overlays ()
    (when (minibufferp)
      (walk-windows
       (lambda (window)
         (with-current-buffer (window-buffer window)
           (when (bound-and-true-p window-stool-mode)
             (window-stool-mode -1)
             (setq-local ii/window-stool-was-active t))))
       'none)))

  (defun ii/window-stool-show-overlays ()
    (message "resetting window stool")
    (walk-windows
     (lambda (window)
       (with-current-buffer (window-buffer window)
         (when (and (not (bound-and-true-p window-stool-mode))
                    (bound-and-true-p ii/window-stool-was-active))
           (window-stool-mode 1)
           (window-stool-single-overlay window (window-start)))))))

  :hook
  (window-stool-mode-hook . (lambda (&rest args)
                              (setq-local ii/window-stool-was-active (bound-and-true-p window-stool-mode))))
  (minibuffer-setup-hook . ii/window-stool-hide-overlays)
  (minibuffer-exit-hook . ii/window-stool-show-overlays)
  :bind
  ("C-w C-a" . ii/window-stool-show-context)
  ("C-c tws" . window-stool-mode))

(use-package drag-stuff
  :bind
  (("M-k" . drag-stuff-up)
   ("M-j" . drag-stuff-down))
  :bind*
  ( :map prog-mode-map
    ("M-k" . drag-stuff-up)
    ("M-j" . drag-stuff-down)))

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
    (let ((buf (buffer-file-name)))
      (ii/when-idle! 5.0 (with-current-buffer buf
                           (flyspell-mode 1)))))

  (defun ii/flyspell-prog-mode-deferred ()
    (let ((buf (buffer-file-name)))
      (ii/when-idle! 5.0 (with-current-buffer buf
                           (flyspell-prog-mode)))))

  :bind
  ( :map goto-map
    ("]," . flyspell-goto-next-error)
    ("[," . +flyspell/goto-prev-error)
    :repeat-map meow-error-repeat-map
    ("," . flyspell-goto-next-error)))

(add-hook 'org-mode-hook 'ii/flyspell-mode-deferred)
(add-hook 'markdown-mode-hook 'ii/flyspell-mode-deferred)
(add-hook 'prog-mode-hook 'flymake-mode)

(setq markdown-fontify-code-blocks-natively t)

(use-package flymake
  :straight nil
  :init
  (setq flymake-show-diagnostics-at-end-of-line nil
        flymake-start-on-flymake-mode nil
        flymake-suppress-zero-counters nil
        flymake-no-changes-timeout 1.0
        flymake-indicator-type 'auto
        flymake-note-bitmap '(large-circle flymake-note-fringe)
        flymake-warning-bitmap '(large-circle flymake-warning-fringe)
        flymake-error-bitmap '(large-circle flymake-error-fringe)
        flymake-fringe-indicator-position 'left-fringe
        flymake-margin-indicator-position 'right-margin
        flymake-margin-indicators-string '((error "•" compilation-error)
                                           (warning "•" compilation-warning)
                                           (note "•" compilation-info))
        flymake-mode-line-counter-format '("("
                                           flymake-mode-line-error-counter
                                           flymake-mode-line-warning-counter
                                           flymake-mode-line-note-counter
                                           ")"))

  (with-eval-after-load 'polymode
    (advice-add 'flymake-mode :around #'polymode-inhibit-in-indirect-buffers))


  (defun ii/flymake-next-note (arg)
    (interactive) "p"
    (flymake-goto-next-error arg '(:note) t))

  (defun ii/flymake-goto-next-warning (arg)
    (interactive "p")
    (flymake-goto-next-error arg '(:warning) t))

  (defun ii/flymake-goto-next-error (arg)
    (interactive "p")
    (flymake-goto-next-error arg '(:error) t))

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
    ;; (+toggle-local-var! flymake-show-diagnostics-at-end-of-line 'normal nil)
    (setq-local flymake-show-diagnostics-at-end-of-line
                (pcase flymake-show-diagnostics-at-end-of-line
                  ('short 'normal)
                  ('normal 'fancy)
                  ('fancy nil)
                  (_ 'short)))
    (when (bound-and-true-p flymake-mode)
      (flymake--update-eol-overlays)
      (flymake--update-diagnostics-listings (current-buffer))))

  :config
  (bind-keys* :map flymake-prefix-map
              ("t" . ii/flymake-toggle-eol))
  :bind-keymap*
  ("C-c !" . flymake-prefix-map)
  :bind
  ( :map flymake-mode-map
    ("C-c t!" . ii/flymake-toggle-eol)
    :map goto-map
    ("]d" . flymake-goto-next-error)
    ("[d" . flymake-goto-prev-error)
    :map flymake-prefix-map
    ("]" . flymake-goto-next-error)
    ("[" . flymake-goto-prev-error)
    ("b" . flymake-show-buffer-diagnostics)
    ("p" . flymake-show-project-diagnostics)
    ("l" . flymake-switch-to-log-buffer)
    :repeat-map meow-error-repeat-map
    ("d" . flymake-goto-next-error)))

;;;; Snippets
(use-package tempel
  :init
  (defmacro ii/tempel-mode-templates! (mode &rest templates)
    "declare TEMPLATES as additional tempel templates for MODE."
    (declare (indent defun))
    (let ((var (intern (concat "ii/tempel-" (symbol-name mode) "-templates"))))
      `(progn
         (when  (not (boundp ',var)) ;; make sure to not override existing templates
           (defvar ,var nil
             ,(format "Templates for %s." (symbol-name mode))))
         ;; (setq ,var ',templates)
         (setq ,var (append ',templates ,var))
         (add-hook ',(intern (concat (symbol-name mode) "-hook"))
                   (lambda ()
                     (add-hook 'tempel-template-sources ',var nil t))))))

  (ii/tempel-mode-templates! emacs-lisp-mode
    (lambda "(lambda (" p ")" n> r> ")")
    (defv "(defvar " p "\n  \"" p "\")")
    (def "(defun " p " (" p ")\n  \"" p "\"" n> r> ")"))

  (ii/tempel-mode-templates! org-mode
    (src "#+begin_src " p n> q n> "#+end_src")
    (exa "#+begin_example " p n> q n> "#+end_example"))

  :bind
  (("C-/ C-e" . tempel-expand)
   ("C-/ C-;" . tempel-complete)
   ("C-c is" . tempel-insert)
   :map tempel-map
   ("TAB" . tempel-next)
   ("<tab>" . tempel-next)
   ("<backtab>" . tempel-previous)))

(use-package tempel-collection
  :after tempel)

;;;; Compiling, running and debugging cod

(setq next-error-recenter 4
      next-error-highlight t
      next-error-highlight-no-select t
      next-line-add-newlines nil
      strokes-use-strokes-buffer nil
      cycle-spacing-actions '(just-one-space delete-space-after delete-space-before delete-all-space restore))

(ii/bind-keys "simple"
  ("M-SPC" . cycle-spacing)
  ("C-x k" . kill-current-buffer)
  ("C-c bK" . kill-current-buffer)
  ("C-c bx" . scratch-buffer)
  ("C-c be" . next-error-select-buffer)
  ("C-c tr" . read-only-mode)
  ("C-c etc" . transpose-chars)
  ("C-c etw" . transpose-words)
  ("C-c etl" . transpose-lines)
  ("C-c ets" . transpose-sexps)
  ("C-c etp" . transpose-paragraphs)
  ("C-c et." . transpose-sentences)
  ("C-c etr" . transpose-regions)
  ("C-c tW" . toggle-word-wrap)
  ("C-c tT" . toggle-truncate-lines)
  :map goto-map
  ("[e" . previous-error)
  ("]e" . next-error)
  ;; (";" . ii/undo-goto-change) TODO: maye implement this
  :repeat-map meow-error-repeat-map
  ("e" . next-error)
  (";" . negative-argument))

(ii/bind-keys "re-builder"
  ("C-c sR" . re-builder))

(setq comint-eol-on-send t
      comint-prompt-read-only t)

(defun ii/comint-shell (ask-shell)
  (interactive "P")
  (comint-run (if ask-shell
                  (read-shell-command "Run: ")
                explicit-shell-file-name)))
(bind-keys
 ("C-c rc" . comint-run)
 ("C-c rs" . ii/comint-shell))

(setq compilation-scroll-output t
      compilation-auto-jump-to-first-error nil
      compilation-max-output-line-length 500
      compilation-ask-about-save nil
      compilation-search-all-directories t
      compilation-context-lines t
      compilation-skip-threshold 0)

(with-eval-after-load 'compile
  (defvar ii/compile-current-root nil)

  (defun ii/compile--before (&rest _args)
    "Setup compilation vars."
    (let ((root (or (project-root (project-current))
                    default-directory)))
      (setq ii/compile-current-root root)))

  (defun ii/compile--after (&rest _args)
    ""
    (setq ii/compile-current-root nil))

  (advice-add 'compile :before #'ii/compile--before)
  (advice-add 'compile :after #'ii/compile--after)

  (setopt compilation-save-buffers-predicate nil))


;; (defmacro ii/compile-register-build-file (name &rest args)
;;   (declare (indent defun))
;;   (let ()
;;     `(progn
;;        (defun ,parser-name ()
;;          ,parser-doc
;;          ,@parser-body)
;;        (add-to-list ))))

(bind-key "C-c cc" 'compile)

(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(use-package fancy-compilation
  :commands (fancy-compilation-mode)
  :after (compile)
  :hook
  (after-init-hook . fancy-compilation-mode))

(use-package dape
  :init
  (setq dape-key-prefix nil
        dape-info-hide-mode-line nil
        dape-buffer-window-arrangement 'right)
  :config
  (dape-breakpoint-load)
  :hook
  (kill-emacs-hook . dape-breakpoint-save))

(use-package cape
  :bind
  (("C-/ C-TAB" . completion-at-point)
   ("C-/ C-<tab>" . completion-at-point)
   ("C-/ C-f" . cape-file)
   ("C-/ C-d" . cape-dict)
   ("C-/ C-h" . cape-history)
   ("C-/ C-a" . cape-abbrev)
   ("C-/ C-/" . cape-dabbrev))
  :hook
  (completion-at-point-functions . cape-file))

(use-package corfu
  :autoload
  (corfu--on)
  :init
  (setq corfu-cycle t
        corfu-echo-delay nil
        corfu-auto-delay 0.05
        corfu-echo-mode nil
        corfu-preselect 'prompt
        corfu-preview-current nil
        corfu-auto nil
        corfu-popupinfo-delay '(0.25 . 0.1)
        corfu-left-margin-width 0
        corfu-right-margin-width 0
        corfu-bar-width 0
        corfu-auto-prefix 2
        corfu-border-width 0
        corfu-count 17
        corfu-max-width 120
        corfu-min-width 60
        corfu-quit-no-match t
        corfu-on-exact-match 'insert
        global-corfu-test-minibuffer nil)

  ;; setup corfu first time something asks
  (ii/eval-on-first-execution completion-at-point
    "corfu"
    :before
    (not (minibufferp))
    (global-corfu-mode 1))

  :config
  (with-eval-after-load 'corfu-popupinfo
    (bind-keys*
     :map corfu-popupinfo-map
     ("C-M-v" . corfu-popupinfo-scroll-up)
     ("C-M-u" . corfu-popupinfo-scroll-down)))

  (defun ii/corfu-toggle-auto ()
    (interactive)
    (when (bound-and-true-p corfu-mode)
      (message "Toggle auto completion.")
      (setq-local corfu-auto (not corfu-auto))
      (corfu-mode -1)
      (corfu-mode 1)))

  (setopt corfu--frame-parameters '((no-accept-focus . t)
                                    (no-focus-on-map . t)
                                    (min-width . t)
                                    (min-height . t)
                                    (border-width . 0)
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
    '((array          :str "[ ]  " :face  font-lock-type-face)
      (boolean        :str "0|1  " :face font-lock-builtin-face)
      (class          :str "cls  " :face font-lock-type-face)
      (color          :str "rgb  " :face success)
      (command        :str "cmd  " :face default)
      (constant       :str "cnst " :face font-lock-constant-face)
      (constructor    :str "cons " :face font-lock-function-name-face)
      (enummember     :str "enum "  :face  font-lock-builtin-face)
      (enum-member    :str  "enum " :face font-lock-builtin-face)
      (enum           :str "enum " :face font-lock-builtin-face)
      (event          :str "evnt " :face font-lock-warning-face)
      (field          :str   "fld  " :face font-lock-variable-name-face)
      (file           :str "file " :face font-lock-string-face)
      (folder         :str "dir  " :face font-lock-doc-face)
      (interface      :str "intf " :face font-lock-type-face)
      (keyword        :str "key  " :face font-lock-keyword-face)
      (macro          :str "macr " :face font-lock-keyword-face)
      (magic          :str "mgc  " :face font-lock-builtin-face)
      (method         :str "mth  " :face font-lock-number-face)
      (function       :str "fun  " :face font-lock-function-name-face)
      (module         :str "mod  " :face font-lock-preprocessor-face)
      (numeric        :str "num  " :face font-lock-builtin-face)
      (operator       :str "op   " :face font-lock-comment-delimiter-face)
      (param          :str "par  " :face default)
      (property       :str "prop " :face font-lock-variable-name-face)
      (reference      :str "ref  " :face font-lock-variable-name-face)
      (snippet        :str "<s>  " :face font-lock-string-face)
      (string         :str "str  " :face font-lock-string-face)
      (struct         :str "{ }  " :face  font-lock-variable-name-face)
      (text           :str "txt  " :face font-lock-doc-face)
      (typeparameter  :str "<T>  " :face font-lock-type-face)
      (type-parameter :str  "<T>  " :face font-lock-type-face)
      (unit           :str "unit " :face font-lock-constant-face)
      (value          :str "val  " :face font-lock-builtin-face)
      (variable       :str "var  " :face font-lock-variable-name-face)
      (t              :str "-----"  :face  font-lock-warning-face)))

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

  ;; (add-to-list 'corfu-margin-formatters 'ii/corfu-margin-formatter)
  :hook
  (global-corfu-mode-hook . corfu-popupinfo-mode)
  :bind
  ( :map corfu-map
    ("M-," . corfu-insert-separator))
  :bind*
  (("C-TAB" . corfu-complete)
   ("C-c ta" . ii/corfu-toggle-auto)
   :map corfu-map
   ("M-h" . corfu-popupinfo-documentation)
   ("M-g" . corfu-info-location)
   ("M-e" . corfu-expand)
   ([tab] . corfu-next)
   ("<tab>" . corfu-next)
   ("TAB" . corfu-next)
   ("C-TAB" . corfu-previous)
   ("C-<tab>" . corfu-previous)))

(use-package sideline-eglot
  :init
  (setq sideline-eglot-code-actions-prefix "! "))

(use-package sideline
  :init
  (setq sideline-backends-left-skip-current-line t
        sideline-backends-right-skip-current-line t
        sideline-order-left 'down
        sideline-delay 0.5
        sideline-order-right 'up
        sideline-format-left "%s   "
        sideline-format-right "%s"
        sideline-priority 100
        sideline-display-backend-name nil
        sideline-backends-right '(sideline-eglot))

  :bind
  (("C-c cS" . sideline-mode)))

;;;; Eglot
(use-package eglot
  :init

  (add-to-list 'ii/load-on-focus-loss-list 'eglot)
  (defun ii/eglot--setup (&rest _args)
    (when eglot-inlay-hints-mode
      (eglot-inlay-hints-mode -1))
    (setq-local eldoc-documentation-strategy 'eldoc-documentation-enthusiast))

  (setopt eglot-autoshutdown t
          eglot-extend-to-xref t
          eglot-sync-connect 1
          eglot-code-action-indications nil
          eglot-codepp-action-indicator "→"
          eglot-prefer-plaintext t
          eglot-confirm-server-edits '(((create rename delete) . diff)
                                       (eglot-code-action-extract . maybe-diff)
                                       (eglot-code-action-rewrite . maybe-diff)
                                       (eglot-code-action-inline . maybe-diff)
                                       (eglot-code-action-quickfix . maybe-diff)
                                       (eglot-code-action-organize-imports . nil)
                                       (t . summary))
          eglot-ignored-server-capabilities
          `(;; :hoverProvider
            ;; :completionProvider
            ;; :signaturehelpprovider
            ;; :definitionProvider
            ;; :typeDefinitionProvider
            ;; :implementationProvider
            ;; :declarationProvider
            ;; :referencesProvider
            :documentHighlightProvider
            ;; :documentSymbolProvider
            ;; :workspaceSymbolProvider
            ;; :codeActionProvider
            ;; :codeLensProvider
            ;; :documentFormattingProvider
            ;; :documentRangeFormattingProvider
            :documentOnTypeFormattingProvider
            ;; :renameProvider
            :documentLinkProvider
            :colorProvider
            :foldingRangeProvider
            ;; :executeCommandProvider
            ;; :inlayHintProvider
            :semanticTokensProvider
            ;; :typeHierarchyProvider
            ;; :callHierarchyProvider
            ;;:diagnosticProvider
            ))
  :config

  (defun ii/eglot-rename (&rest args)
    (interactive)
    (let ((case-fold-search nil))
      (command-execute 'eglot-rename )))

  (add-to-list 'eglot-server-programs '(haskell-ts-mode "haskell-language-server-wrapper" "--lsp"))

  ;; rassumfrassum setup
  (when nil
    (defvar rass-from-git nil
      "Non-nil if the rassumfrassum multiplexer is installed from github.")

    (defvar rass-command "rass")

    (defvar rass-directory (expand-file-name "./local/rassumfrassum" user-emacs-directory))

    (defmacro ii/rass-eglot-cmd (servers &rest options)
      (cond ((stringp rass-command)
             `(,rass-command ,@options ,@(mapcar (lambda (server) ) servers)))))

    (when (not (executable-find "rass"))
      (message "rass not found,")
      (when (and (executable-find "python")
                 (executable-find "git"))
        (message "Downloading rass")
        (setq rass-from-git t)
        (let ((proc (start-process-shell-command "rass-download" (get-buffer-create " *rass-download") (format "git clone --depth 1 https://github.com/joaotavora/rassumfrassum %s" rass-directory))))
          (set-process-sentinel proc
                                (lambda (proc out)
                                  (pcase (process-status proc)
                                    ('exit )))))
        (setq rass-command (format "PYTHONPATH=%s/src python -m rassumfrassum" rass-directory)))))


  (when (executable-find "rass")
    (add-to-list 'eglot-server-programs '((python-ts-mode python-mode) "rass" "python"))
    (add-to-list 'eglot-server-programs '((rust-ts-mode rust-mode) "rass" "--" "rust-analyzer"))
    (add-to-list 'eglot-server-programs '((c-mode c-ts-mode) "ccls"))
    (add-to-list 'eglot-server-programs '((typescript-ts-mode tsx-ts-mode)
                                          "rass"
                                          "--" "typescript-language-server" "--stdio"
                                          "--" "eslint" "--stdio"
                                          "--" "tailwindcss-language-server" "--stdio")))
  :bind
  ( :map eglot-mode-map
    ("M-g R"  . ii/eglot-rename)
    ("M-g y"  . eglot-find-typeDefinition)
    ("C-c cr" . ii/eglot-rename)
    ("C-c ch" . eglot-show-call-hierarchy)
    ("C-c ct" . eglot-show-type-hierarchy)
    ("C-c cf" . eglot-format)
    ("C-c ci" . eglot-inlay-hints-mode)
    ("C-c ca" . eglot-code-actions)))

;;; Indent indicators
(setq whitespace-global-modes '(not fundamental-mode special-mode image-mode nov-mode pdf-view-mode archive-mode markdown-mode gfm-mode org-mode latex-mode dired-mode csv-mode nxml-mode ess-mode diff-mode wdired-mode magit-mode magit-diff-mode)
      whitespace-display-mappings `((space-mark 32
                                                [,(pcase +base/font-family
                                                    ("Comic ShannsMono Nerd Font Mono" ?•)
                                                    ("Comic Code Ligatures" 183))]
                                                [46])
                                    (space-mark 160 [164] [95])
                                    (newline-mark 10 [36 10])
                                    (tab-mark 9 [187 9] [92 9]))
      whitespace-style '(face tab-mark space-mark spaces page-delimiters newline indentation)
      whitespace-line-column nil)

(bind-key "C-c tS" #'whitespace-toggle-options)

(add-hook 'before-save-hook #'whitespace-cleanup)

(ii/eval-on-first-hook find-file-hook "global-whitespace-mode" t
  (add-hook 'whitespace-mode-hook #'delete-trailing-whitespace-mode)
  (global-whitespace-mode 1))

(setq outline-minor-mode-cycle nil
      outline-minor-mode-cycle-filter nil
      outline-default-state nil
      outline-minor-mode-highlight nil
      outline-blank-line t)

(with-eval-after-load 'outline
  (defvar ii/outline-minor-mode-ellipsis "... "
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
    (ii/outline-minor-mode--set-elipsis ii/outline-minor-mode-ellipsis)))

(add-hook 'grep-mode-hook 'outline-minor-mode)
(add-hook 'rg-mode-hook 'outline-minor-mode)
(add-hook 'prog-mode-hook 'outline-minor-mode)

(add-hook 'outline-minor-mode-hook 'ii/outline-minor-mode--setup)

(ii/bind-keys "outline"
  :map outline-minor-mode-map
  ("M-TAB" . outline-cycle)
  ("M-g [@" . outline-previous-heading)
  ("M-g ]@" . outline-next-heading)
  :repeat-map outline-repeat-map
  ("TAB" . outline-cycle))

(use-package indent-bars
  :straight (indent-bars :type git
                         :host github
                         :repo "jdtsmith/indent-bars")
  :commands
  indent-bars-mode
  :init

  (with-eval-after-load 'polymode
    (advice-add 'indent-bars-mode :around #'polymode-inhibit-in-indirect-buffers))

  (setq indent-bars-color '(highlight :face-bg t :blend 0.5)
        indent-bars-starting-column 0
        indent-bars-treesit-support t
        indent-bars-display-on-blank-lines t
        indent-bars-pattern "."
        indent-bars-no-descend-lists t
        indent-bars-display-on-blank-lines t
        indent-bars-no-descend-string t
        indent-bars-prefer-character t)

  (defvar ii/indent-bars-exclude-modes '(emacs-lisp-mode
                                         lisp-data-mode
                                         lisp-interaction-mode
                                         clojure-mode
                                         lisp-mode
                                         html-ts-mode
                                         html-mode
                                         mhtml-mode
                                         mhtml-ts-mode
                                         python-ts-mode
                                         python-mode
                                         common-lisp-mode
                                         haskell-mode
                                         haskell-ts-mode)
    "TODO")
  (defun ii/indent-bars--maybe-turn-on ()
    (when (not (memq major-mode ii/indent-bars-exclude-modes))
      (whitespace-toggle-options '(space-mark))
      (indent-bars-mode 1)))
  ;;  (add-hook 'prog-mode-hook 'ii/indent-bars--maybe-turn-on)
  )

(use-package project
  :init
  (defun ii/project--root-finder (dir)
    "Integrate .git project roots."
    (let ((dotgit (and (setq gitdir (locate-dominating-file dir ".git"))
                       (expand-file-name gitdir)))
          (p (and (setq pdir (locate-dominating-file dir ".project"))
                  (expand-file-name pdir))))
      (if (or dotgit p)
          (cons 'transient (file-name-directory
                            (if (> (length dotgit) (length p))
                                dotgit
                              p))))))

  (defun ii/project-occur (regexp n)
    (interactive (list (read-regexp "Search for: ") (prefix-numeric-value prefix-arg)))
    (if-let* ((p (project-current t)))
        (multi-occur (seq-filter #'buffer-file-name
                                 (project-buffers p))
                     regexp n)
      (message "Project not found.")))

  (defun ii/project-multi-isearch (ignore-mode)
    (interactive "p")
    (let ((bufs (match-buffers (if (not ignore-mode) `(derived-mode . ,major-mode) t)
                               (seq-filter #'buffer-file-name
                                           (project-buffers (project-current t))))))
      (multi-isearch-buffers bufs)))

  (defun ii/project-multi-isearch-regexp (ignore-mode)
    (interactive "p")
    (let ((bufs (match-buffers (if (not ignore-mode) `(derived-mode . ,major-mode) t)
                               (seq-filter #'buffer-file-name
                                           (project-buffers (project-current t))))))
      (multi-isearch-buffers-regexp bufs)))

  (defun ii/project-multi-isearch-files (ignore-mode)
    (interactive "p")
    (let* ((ext (file-name-extension buffer-file-name))
           (files (seq-filter (if (or ignore-mode (null ext))
                                  #'identity
                                (lambda (file)
                                  (equal (file-name-extension file) ext)))
                              (project-files (project-current t)))))
      (multi-isearch-files files)))

  (defun ii/project-multi-isearch-files-regexp (ignore-mode)
    (interactive "p")
    (let* ((ext (file-name-extension buffer-file-name))
           (files (seq-filter (if (or ignore-mode (null ext))
                                  #'identity
                                (lambda (file)
                                  (equal (file-name-extension file) ext)))
                              (project-files (project-current t)))))
      (multi-isearch-files-regexp files)))

  (defun ii/project-scratch-buffer (project)
    (interactive (list (project-current)))
    (when project
      (let ((buffer (get-buffer-create (format " *scratch-%s*" (project-name project)))))
        (with-current-buffer buffer
          (setq-local kill-buffer-query-functions (list )))
        (select-window (display-buffer buffer)))))

  (defun ii/project-magit-status ()
    (interactive)
    (magit-status (project-root (project-current))))

  (defun ii/project-list-buffers-consult (_project _files-only)
    (interactive)
    (consult-project-buffer))

  (defun ii/project-find-dired ()
    (interactive)
    (ii/with-project-root! dir
      (funcall-interactively 'find-dired dir (read-regexp "Find-grep (grep regexp): "))))

  (defun ii/project-find-grep-dired ()
    (interactive)
    (ii/with-project-root! dir
      (funcall-interactively 'find-grep-dired dir (read-regexp "Find-grep (grep regexp): "))))

  (defun ii/project-find-name-dired ()
    (interactive)
    (ii/with-project-root! dir
      (funcall-interactively 'find-name-dired dir (read-string "Find-name (filename wildcard): "))))

  (defun ii/project-comint-shell ()
    (interactive)
    (ii/with-project-root! _
      (comint-run explicit-shell-file-name)))

  (add-hook 'project-find-functions 'ii/project--root-finder)

  (setopt project-buffers-viewer 'ii/project-list-buffers-consult
          project-compilation-buffer-name-function 'project-prefixed-buffer-name
          project-vc-extra-root-markers '("package.json" "cargo.toml" ".+\.cabal"))
  (with-eval-after-load 'project
    (add-to-list 'project-switch-commands '(project-dired "Open root dir"))
    (add-to-list 'project-switch-commands '(ii/project-magit-status "Magit status" ?m))
    (add-to-list 'project-switch-commands '(affe-find "Fuzzy Find file" ?F))
    (add-to-list 'project-switch-commands '(eat-project "Eat" ?E))
    (add-to-list 'project-switch-commands '(affe-grep "Fuzzy find rx" ?/)))
  :bind
  ( :map project-prefix-map
    ("s" . ii/project-comint-shell)
    ("C-s" . ii/project-multi-isearch)
    ("C-M-s" . ii/project-multi-isearch-regexp)
    ("O" . ii/project-occur)
    ("X" . ii/project-scratch-buffer)
    ("d/" . ii/project-find-dired)
    ("d*" . ii/project-find-grep-dired)
    ("dn" . ii/project-find-name-dired)))

(use-package envrc
  :defer 4
  :bind
  ( :map envrc-mode-map
    ("C-c rr" . envrc-reload)
    ("C-c ra" . envrc-allow)
    ("C-c rl" . envrc-show-log)
    ("C-c r." . envrc-reload-all))
  :config
  (envrc-global-mode 1))

(use-package docker
  :bind
  (("C-c odd" . docker)
   ("C-c odi" . docker-images)
   ("C-c odv" . docker-volumes)))

(use-package kele
  :config
  (kele-mode 1)
  :bind
  (("C-c ok" . kele-dispatch)))

;; Version Control
(setopt vc-make-backup-files nil
        vc-display-status t
        vc-follow-symlinks t
        vc-allow-async-revert t
        vc-allow-async-diff t
        vc-dir-save-some-buffers-on-revert t
        vc-display-failed-async-commands t
        vc-annotate-background-mode t
        vc-annotate-display-mode 'scale
        vc-annotate-color-map '((20 . "#73c936") (40 . "#a1cf35") (60 . "#d0d634") (80 . "#ffdd33")
                                (100 . "#f9af31") (120 . "#f48130") (140 . "#ef532f") (160 . "#d16e61")
                                (180 . "#b38a94") (200 . "#96a6c8") (220 . "#b5819b") (240 . "#d45c6e")
                                (260 . "#f43841") (280 . "#d94c51") (300 . "#bf6161") (320 . "#a57672")
                                (340 . "#5c5e5e") (360 . "#5c5e5e"))
        diff-font-lock-syntax t
        custom-magic-show nil
        diff-refine 'navigation
        diff-refine-nonmodified t
        diff-font-lock-prettify t
        vc-directory-exclusion-list (append (or (when (boundp 'vc-directory-exclusion-list)
                                                  vc-directory-exclusion-list)
                                                '())
                                            '("straight" "node_modules" "build")))

(ii/bind-keys "vc"
  :map vc-prefix-map
  ("d" . project-vc-dir))

(use-package vc-jj)

(defun ii/smerge-maybe-start ()
  "Start `smerge-mode' if current file buffer contains merge markers."
  (save-mark-and-excursion
    (goto-char (point-min))
    (when (search-forward-regexp "^<<<<<<< " nil t 1)
      (smerge-mode 1))))

(add-hook 'find-file-hook #'ii/smerge-maybe-start t)

(setq ediff-shell (+os/per-system! :linux "sh" :wsl "sh")
      ediff-ignore-case t
      ediff-no-emacs-help-in-control-buffer t
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; (defvar ii/ediff-saved-window-configuration nil
;;   "Window configuration in uise before starting `ediff'.")
;;
;; (defun ii/ediff-reset-window-configuration ()
;;   (set-window-configuration ii/ediff-saved-window-configuration)
;;   (setq ii/ediff-saved-window-configuration nil))
;;
;; (defun ii/ediff-setup-windows (buffer-a buffer-b buffer-c control-buffer)
;;   (setq ii/ediff-saved-window-configuration (current-window-configuration))
;;   (delete-other-windows)
;;   (setq ediff-select-control-window-on-setup nil)
;;   (set-window-dedicated-p (frame-root-window) nil)
;;   (let* ((window-A nil)
;;          (window-B nil)
;;          (window-C nil))
;;     (cond
;;      ;; setup for merge
;;      ((bound-and-true-p ediff-merge-job)
;;       (setq window-A ))
;;      ;; setup for compare
;;      (t
;;       (setq window-A (split-window-right)
;;             window-B (prog1
;;                          (other-window)
;;                        (display-buffer buffer-b)))
;;       (balance-windows)))
;;     (setq ediff-window-A window-A
;;           ediff-window-B window-B
;;           ediff-window-C window-C
;;           ;; dont show control window
;;           ediff-control-window nil)))

(use-package git-modes)

(use-package git-timemachine
  :bind
  (("C-c g tt" . git-timemachine-toggle)
   :map git-timemachine-mode-map
   ("C-c g tb" . git-timemachine-blame)
   ("C-c g tn" . git-timemachine-show-next-revision)
   ("C-c g tp" . git-timemachine-show-previous-revision)))

(use-package magit
  :preface
  (setq magit-auto-revert-mode nil)
  :init
  ;; load on next idle period
  (add-to-list 'ii/load-on-focus-loss-list 'magit)
  (setq forge-add-default-bindings t
        magit-git-executable (+os/per-system! :wsl "git"
                                              :linux "git"
                                              :win "C:/Program Files/Git/cmd/git.exe")
        magit-diff-refine-hunk t
        magit-diff-highlight-hunk-body nil
        magit-diff-paint-whitespace 'uncomitted
        magit-diff-paint-whitespace-lines nil
        magit-log-auto-more t
        magit-log-color-graph-limit 512
        magit-log-section-commit-count 15)
  :hook
  (magit-process-find-password-functions . magit-process-password-auth-source)
  :bind
  (("C-c gg" . magit-status)
   ("C-c gb" . magit-blame)
   ("C-c gd" . magit-diff)
   ("C-c gx" . magit-dispatch)
   ("C-c gi" . magit-gitignore-in-topdir)))

(use-package magit-gh
  :bind
  (("C-c gh" . magit-gh)))

(use-package diff-hl
  :init
  (setq diff-hl-show-hunk-function 'diff-hl-show-hunk-inline-popup
        diff-hl-show-hunk-inline-popup-hide-hunk t
        diff-hl-show-hunk-inline-popup-smart-lines nil
        diff-hl-draw-borders nil
        diff-hl-update-async t
        ii/diff-hl-repeat-map (make-sparse-keymap)
        diff-hl-margin-symbols-alist '((insert . "+")
                                       (delete . "-")
                                       (change . "=")
                                       (unknown . "?")
                                       (ignored . "i"))
        diff-hl-reference-revision nil)

  (ii/eval-on-first-hook find-file-hook "diff-hl" t (global-diff-hl-mode 1))
  :config

  (defvar-local ii/diff-hl-popup-window nil)

  (defun ii/diff-hl-show-hunk-popup-window (buffer &optional _line)
    (let ((popup-window (display-buffer-pop-up-window buffer nil)))
      (setq-local ii/diff-hl-popup-window popup-window)))

  (defun ii/diff-hl-select-diff-window ()
    (interactive)
    (when (windowp ii/diff-hl-popup-window)
      (select-window ii/diff-hl-popup-window)))

  (defun ii/diff-hl-toggle-whitespace-display (&rest args)
    "Toggle off `whitespace-mode' when `diff-hl' popup is shown."
    (cond ((bound-and-true-p diff-hl-inline-popup-transient-mode)
           (whitespace-mode -1))
          (t
           (when (not (memq this-command '(diff-hl-next-hunk diff-hl-show-hunk-next diff-hl-show-hunk-previous diff-hl-previous-hunk)))
             (whitespace-mode 1)))))

  (add-hook 'diff-hl-inline-popup-transient-mode-hook 'ii/diff-hl-toggle-whitespace-display)

  :bind
  (("M-g \]g" . diff-hl-next-hunk)
   ("M-g \[g" . diff-hl-previous-hunk)
   :repeat-map ii/diff-hl-repeat-map
   ("n" . diff-hl-show-nex))
  :hook
  (dired-mode-hook . diff-hl-dired-mode)
  (global-diff-hl-mode-hook . diff-hl-margin-mode))

;; Terminal Emulation

(setq eshell-banner-message "")

(use-package ghostel
  :straight ( :type git
              :host github
              :repo "dakra/ghostel")
  :init
  (setopt ghostel-max-scrollback (* 10 1000 1000)
          ghostel-enable-url-detection nil
          ghostel-enable-file-detection nil)
  (add-hook 'after-init-hook 'ghostel-compile-global-mode)
  :config

  (with-eval-after-load 'eshell
    (ghostel-eshell-visual-command-mode 1))

  :bind
  (("C-c og" . ghostel)
   ("C-c cg" . ghostel-compile)
   :map project-prefix-map
   ("g" . ghostel-project)))

(use-package eat
  :if (eq system-type 'gnu/linux)
  :init
  (setq eat-shell (+os/per-system! :linux "zsh" :win explicit-shell-file-name)
        eat-enable-blinking-text nil
        eat-term-scrollback-size (* 1000 1000)
        eat-enable-shell-prompt-annotation nil
        eat-minimum-latency 0.002)
  ;;; eat sets the terminfo directory to the straight build path, which contains only source and emacs Info files. This points it to the actual repo.
  (setq eat-term-terminfo-directory (expand-file-name "straight/repos/eat/terminfo" user-emacs-directory))

  (ii/defhook! ii/fixup-eat ()
    "Set `eat' local vars."
    :hook-var eat-mode-hook
    (setq-local process-adaptive-read-buffering t))

  (when (eq system-type 'gnu/linux)
    (ii/eval-on-first-hook eat-mode-hook "eat-eshell-mode" t (eat-eshell-mode 1)))
  :bind
  (("C-c oe" . eat)
   :map project-prefix-map
   ("e" . eat-project)
   :map meow-eat-toggle-map
   ("l" . eat-line-mode)
   ("c" . eat-char-mode)
   ("s" . eat-semi-char-mode)
   ("e" . eat-emacs-mode)))

(use-package powershell)

;;;; Buffer Management
(setq auto-revert-verbose t
      auto-revert-use-notify nil
      auto-revert-stop-on-user-input nil
      revert-without-query (list "."))
;; taken from doom emacs
;; https://github.com/doomemacs/doomemacs/blob/57818a6da90fbef39ff80d62fab2cd319496c3b9/lisp/doom-editor.el#L243
(defun ii/auto-revert-buffer (&optional _)
  "Auto revert current buffer if necessary."
  (require 'autorevert)
  (unless (or (bound-and-true-p auto-revert-mode)
              (active-minibuffer-window)
              (and buffer-file-name
                   auto-revert-remote-files
                   (file-remote-p buffer-file-name nil t)))
    (let ((auto-revert-mode t))
      (auto-revert-handler))))

(defun ii/auto-revert-setup ()
  (add-hook 'window-selection-change-functions #'ii/auto-revert-buffer)
  (add-hook 'window-buffer-change-functions #'ii/auto-revert-buffer)
  (remove-hook 'after-init-hook #'ii/auto-revert-setup))

(add-hook 'after-init-hook 'ii/auto-revert-setup)

(defun ii/new-scratch-buffer (name dont-display)
  (interactive (list (read-string "Buffer name: ") prefix-arg))
  (let ((buf (generate-new-buffer (format "*%s*" name))))
    (with-current-buffer buf
      (emacs-lisp-mode)
      (set-buffer-modified-p t))
    (unless prefix-arg
      (select-window (display-buffer buf)))))

(bind-key "C-c bN" #'ii/new-scratch-buffer)

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
                        (mode . ghostel-mode)
                        (mode . eat-mode)
                        (mode . eshell-mode)))
           ("help" (or
                    (name . "^\\*Help\\*$")
                    (name . "^\\*info\\*$")
                    (name . "^\\*helpful"))))))


  :config
  (setq ibuffer-human-readable-size t)

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

  (defun ii/ibuffer ()
    (interactive)
    (let ((prev-window (selected-window)))
      (when (ibuffer-other-window)
        (with-current-buffer (window-buffer (selected-window))
          (setq-local ii/ibuffer-prev-window prev-window)))))

  :bind
  (("C-c bi" . ii/ibuffer)
   :map ibuffer-mode-map
   ("/^" . ibuffer-pop-filter)))

(use-package ibuffer-vc)

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude ".*[0-9a-z]+\.\\(png\\|jpg\\|jpeg\\|webp\\|svg\\|gif\\)")
  (add-to-list 'recentf-exclude ".*\.priv/.*"))

(setq find-sibling-rules
      `(("[^/]\.c" "\\\\1.h")))

(defun ii/buffer-toggle-modified ()
  (interactive)
  (set-buffer-modified-p (not (buffer-modified-p))))

(bind-key "C-c b~" #'ii/buffer-toggle-modified)

(ii/bind-keys "files"
  ("C-c br" . revert-buffer)
  ("C-c bR" . revert-buffer-with-fine-grain)
  ("C-c dc" . copy-directory)
  ("C-c dd" . delete-directory)
  ("C-c dm" . make-directory)
  ("C-c dp" . pwd)
  ("C-c if" . insert-file)
  ("C-c iF" . insert-file-literally)
  ("C-c fa" . find-alternate-file)
  ("C-c fR" . rename-visited-file)
  ("C-c f^" . recover-this-file)
  ("C-c fs" . find-sibling-file))

(ii/bind-keys "files-x"
  ("C-c ilp" . modify-file-local-variable-prop-line)
  ("C-c flv" . modify-file-local-variable)
  ("C-c dla" . add-dir-local-variable)
  ("C-c dld" . delete-dir-local-variable)
  ("C-c dlc" . copy-file-locals-to-dir-locals)
  ("C-c dlf" . copy-dir-locals-to-file-locals)
  ("C-c dlF" . copy-dir-locals-to-file-locals-prop-line))

(use-package dired
  :straight nil
  :defer t
  :init
  (setq dired-listing-switches "-alh --group-directories-first"
        dired-recursive-copies 'always
        dired-recursive-deletes 'top
        dired-kill-when-opening-new-dired-buffer nil
        dired-auto-revert-buffer t
        dired-create-destination-dirs 'ask
        dired-auto-revert-buffer 'dired-buffer-stale-p
        dired-omit-verbose nil
        dired-vc-rename-file t
        dired-clean-confirm-killing-deleted-buffers nil)

  :bind
  (("M-g M-d" . dired-at-point)
   ("C-c d." . dired-at-point)
   ("C-c df" . dired)
   ("C-c dj" . dired-jump)
   :map dired-mode-map
   ("e" . wdired-change-to-wdired-mode)
   ("-" . dired-up-directory)))

(use-package wdired
  :straight nil
  :init
  (setq wdired-allow-to-change-permissions t
        wdired-use-dired-vertical-movement 'sometimes))

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
        dired-filter-verbose nil
        dired-subtree-use-backgrounds nil
        dired-subtree-line-prefix "  │")

  (defun ii/dired-subtree--after-insert-advice ()
    "Advice for `dired-subtree--after-insert' to deal with nonexistant overlays."
    (if (fboundp 'dired-insert-set-properties)
        (let ((inhibit-read-only t)
              (ov (dired-subtree--get-ov)))
          (dired-insert-set-properties (overlay-start ov) (overlay-end ov)))
      (when (featurep 'dired-details)
        (dired-details-delete-overlays)
        (dired-details-activate))))

  :bind
  ( :map dired-mode-map
    ("r c" . dired-ranger-copy)
    ("r p" . dired-ranger-paste)
    ("r m" . dired-ranger-move)
    ("TAB" . dired-subtree-toggle)
    ("<tab>" . dired-subtree-toggle))
  :hook
  (dired-mode-hook . dired-filter-mode))

(use-package dired-sidebar
  :init
  (setq dired-sidebar-theme nil
        dired-sidebar-should-follow-file nil
        dired-sidebar-skip-subtree-parent t
        dired-sidebar-follow-file-idle-delay 0.5
        dired-sidebar-refresh-on-special-commands nil
        dired-sidebar-no-delete-other-windows t
        dired-sidebar-pop-to-sidebar-on-toggle-open nil)

  (defun ii/dired-sidebar-maybe-follow (&rest _args)
    (when (dired-sidebar-showing-sidebar-p)
      (dired-sidebar-follow-file)))

  (with-eval-after-load 'dired-sidebar
    (add-hook 'window-selection-change-functions #'ii/dired-sidebar-maybe-follow)
    (add-hook 'window-buffer-change-functions #'ii/dired-sidebar-maybe-follow)
    (add-hook 'find-file-hook #'ii/dired-sidebar-maybe-follow))

  :bind
  (("C-c tD" . dired-sidebar-toggle-sidebar)))

;;;; remote
(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc"))

(with-eval-after-load 'tramp
  ;; change sudo timeout
  (add-to-list 'tramp-connection-properties '(nil
                                              "session-timeout"
                                              "240")))

(use-package tramp-rpc
  :straight ( :type git
              :host github
              :repo "ArthurHeymans/emacs-tramp-rpc"))

(use-package implicit-org
  :straight `(implicit-org :type nil
                           :local-repo ,(expand-file-name "org" ii/elisp-path))
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
        +org/birthdays-file (substitute-in-file-name "$HOME/org/agenda/birthdays.org")
        org-hide-emphasis-markers 1
        org-directory (expand-file-name "~/org/")
        org-hide-macro-markers 1
        org-latex-packages-alist '(("" "color" t) ("" "tikz" t))
        org-confirm-babel-evaluate nil
        org-md-headline-style 'setext
        org-odt-preferred-output-format "doc"
        org-return-follows-link t
        org-use-fast-todo-selection 'expert
        ;; agenda
        org-agenda-restore-windows-after-quit t
        org-log-done 'time
        org-log-into-drawer t
        org-refile-targets '(("archive.org" :maxlevel . 1)
                             ("agenda.org" :maxlevel . 1)
                             ("tasks.org" :maxlevel . 1))
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANCELLED(k@)")))

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
        ;; tasks
        `(;; agenda
          ("a" "Agenda entry" entry
           (file ,+org/agenda-file)
           "* %^t %i"
           :kill-buffer)
          ;; add a birthday
          ("b" "Birthday" entry (file ,+org/birthdays-file)
           "* %^t %i"
           :kill-buffer)
          ;; add a general task
          ("t" "General task" entry (file+olp ,+org/tasks-file "TASKS")
           "* TODO %?\n %U\n %i"
           :prepend
           :kill-buffer)

          ;; tasks related to current file
          ("f" "File-specific task")
          ("ft" "TODO" entry (file+olp ,+org/tasks-file "TASKS")
           "* TODO %F: %?\n %i")
          ("fb" "BACKLOG" entry (file+olp ,+org/tasks-file "TASKS")
           "* BACKLOG %F: %?\n %i")
          ("fp" "PLAN" entry (file+olp ,+org/tasks-file "TASKS")
           "* PLAN %F: %?\n %i")
          ("fr" "READY" entry (file+olp ,+org/tasks-file "TASKS")
           "* READY %F: %?\n %i")
          ("fa" "ACTIVE" entry (file+olp ,+org/tasks-file "TASKS")
           "* ACTIVE %F: %?\n %i")
          ("fv" "REVIEW" entry (file+olp ,+org/tasks-file "TASKS")
           "* REVIEW %F: %?\n %i")
          ("fw" "WAIT" entry (file+olp ,+org/tasks-file "TASKS")
           "* WAIT %F: %?\n %i")
          ("fh" "HOLD" entry (file+olp ,+org/tasks-file "TASKS")
           "* HOLD %F: %?\n %i")
          ("fc" "COMPLETED" entry (file+olp ,+org/tasks-file "TASKS")
           "* COMPLETED %F: %?\n %i")
          ("fk" "CANCELLED" entry (file+olp ,+org/tasks-file "TASKS")
           "* CANCELLED %F: %?\n %i")

          ;; tasks related to current project
          ("p" "Project-specific tasks")
          ("pt" "TODO" entry (file+olp ,+org/tasks-file "TASKS")
           "* TODO %(project-root (project-current t)): %?\n %i")
          ("pb" "BACKLOG" entry (file+olp ,+org/tasks-file "TASKS")
           "* BACKLOG %(project-root (project-current t)): %?\n %i")
          ("pp" "PLAN" entry (file+olp ,+org/tasks-file "TASKS")
           "* PLAN %(project-root (project-current t)): %?\n %i")
          ("pr" "READY" entry (file+olp ,+org/tasks-file "TASKS")
           "* READY %(project-root (project-current t)): %?\n %i")
          ("pa" "ACTIVE" entry (file+olp ,+org/tasks-file "TASKS")
           "* ACTIVE %(project-root (project-current t)): %?\n %i")
          ("pv" "REVIEW" entry (file+olp ,+org/tasks-file "TASKS")
           "* REVIEW %(project-root (project-current t)): %?\n %i")
          ("pw" "WAIT" entry (file+olp ,+org/tasks-file "TASKS")
           "* WAIT %(project-root (project-current t)): %?\n %i")
          ("ph" "HOLD" entry (file+olp ,+org/tasks-file "TASKS")
           "* HOLD %(project-root (project-current t)): %?\n %i")
          ("pc" "COMPLETED" entry (file+olp ,+org/tasks-file "TASKS")
           "* COMPLETED %(project-root (project-current t)): %?\n %i")
          ("pk" "CANCELLED" entry (file+olp ,+org/tasks-file "TASKS")
           "* CANCELLED %(project-root (project-current t)): %?\n %i")

          ;; journal
          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree ,+org/journal-file)
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)))

  (defun ii/agenda-title (arg &rest args)
    "custom"
    (let ((inhibit-read-only t))
      (insert arg)))


  (setopt org-agenda-files (list (expand-file-name "agenda/" org-directory))
          org-agenda-skip-unavailable-files t
          org-agenda-breadcrumbs-separator ">"
          org-agenda-start-with-log-mode t
          org-agenda-span 'month
          org-agenda-custom-commands
          '(("d" "Dashboard"
             ((ii/agenda-title "DASHBOARD")
              (agenda "" ((org-deadline-warning-days 7)))
              (todo "NEXT" ((org-agenda-overriding-header "Next Tasks")))
              (todo "TODO" nil)
              (todo "ACTIVE"
                    ((org-agenda-overriding-header "Active Projects")
                     (org-agenda-files org-agenda-files)))
              (todo "HOLD" ((org-agenda-overriding-header "Projects On Hold")))))

            ("n" "Next Tasks"
             ((ii/agenda-title "NEXT TASKS")
              (todo "NEXT"
                    ((org-agenda-overriding-header "Next Tasks")))))

            ;; Low-effort next actions
            ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
             ((org-agenda-overriding-header "Low Effort Tasks")
              (org-agenda-max-todos 20)
              (org-agenda-files org-agenda-files)))

            ;; file-specific status
            ))

  (add-to-list 'ii/load-on-focus-loss-list 'org)

  :config

  (defun ii/org-capture-file-task ()
    "Capture a file task"
    (interactive)
    (org-capture nil "f"))

  (defun ii/org-capture-project-task ()
    "Capture a project task."
    (interactive)
    (org-capture nil "p"))

  (defun ii/browse-agenda-files ()
    (interactive)
    (affe-find (expand-file-name "agenda/" org-directory)))

  (defun ii/org-project-tasks (project-root)
    (interactive (list (project-root (project-current t))))
    (let ((org-agenda-regexp-filter (list project-root)))
      (org-agenda nil "p")))

  (defun ii/org-file-tasks (file)
    (interactive (list (buffer-file-name)))
    (let ((old org-agenda-regexp-filter))
      (setq org-agenda-regexp-filter (list file))
      (org-agenda nil "f")))

  (defun ii/org-maybe-update-agenda ()
    "Update existing agenda views if changing todo state in agenda files."
    (when (member default-directory org-agenda-files)
      (when-let* ((buf (get-buffer org-agenda-buffer-name)))
        (with-current-buffer buf
          (org-agenda-redo-all t)))
      (save-buffer)))

  (defun ii/org-summary-todo (done not-done)
    (let ((org-log-done org-todo-log-states))
      (org-todo (if (= not-done 0) "DONE" "TODO")))
    (org-save-all-org-buffers))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-refile :after 'org-save-all-org-buffers)
  ;; (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)

  (add-hook 'org-after-todo-state-change-hook #'ii/org-maybe-update-agenda)
  (add-hook 'org-after-todo-statistics-hook #'ii/org-summary-todo)
  :bind
  (("C-c oa" . org-agenda)
   ("C-c oc" . org-capture)
   ("C-c oT" . org-todo-list)
   ("C-c ft" . ii/org-file-tasks)
   ("C-c fA" . ii/browse-agenda-files)
   :map project-prefix-map
   ("t" . ii/org-project-tasks))
  :hook
  (org-mode-hook . visual-line-mode)
  (org-mode-hook . org-indent-mode)
  (org-mode-hook . org-latex-preview-mode))

(use-package org-gcal)

(use-package org-download)

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
   "The places where I keep my #+documentation"))

(use-package org-transclusion
  :after (org))

(use-package org-roam
  :custom
  (org-roam-directory (substitute-in-file-name "$HOME/org/roam"))
  (org-roam-dailies-directory "daily")
  (org-roam-dailies-capture-templates '(("d" "default" entry
                                         "* %?"
                                         :target (file+head "%<%Y-%m-%d>.org"
                                                            "#+title: %Y=%m-%d\n"))))
  :init
  (add-to-list 'ii/load-on-focus-loss-list 'org-roam)
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
  :bind
  (("C-c nf" . org-roam-node-find)
   ("C-c nr" . org-roam-node-random)
   ("C-c nc" . org-roam-capture)
   ("C-c n*" . +org/rg-in-roam-notes)
   ("C-c ns" . org-roam-db-sync)))

(use-package gnuplot)

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
          (shell . t)
          (dot . t)
          (org  .t)
          (latex . t)
          (scheme . t)
          (sass . t)
          (haskell . t)
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
  :bind
  (("C-c oC" . calibredb)))

(use-package annotate
  ;; unbind default keybindings
  :bind
  (("C-c tn" . annotate-mode)
   :map annotate-mode-map
   ("C-c C-a" . nil)
   ("C-c C-c" . nil)
   ("C-c C-d" . nil)
   ("C-c C-p" . nil)
   ("C-c C-s" . nil)
   ("C-c \[" . nil)
   ("C-c \]" . nil)
   ("C-c C-a a" . annotate-annotate)
   ("C-c C-a i" . annotate-integrate-annotations)
   ("C-c C-a c" . annotate-clear-annotations)
   ("C-c C-a d" . annotate-delete-annotation)
   ("C-c C-a s" . annotate-save-annotations)
   ("C-c C-a t" . annotate-toggle-annotation-text)
   ("C-c C-a T" . annotate-toggle-all-annotations-text)
   ("C-c C-a n" . annotate-goto-next-annotation)
   ("C-c C-a p" . annotate-goto-previous-annotation)
   ("C-c C-a e" . annotate-export-annotations)
   ("C-c C-a C" . annotate-change-annotation-colors))
  :hook
  ((prog-mode markdown-mode markdown-ts-mode org-mode) . annotate-mode))

(use-package org-noter
  :custom
  (org-noter-default-notes-file-names nil)
  (org-noter-notes-search-path `(,(substitute-in-file-name "~/org/roam/booknotes/")))
  (org-noter-default-heading-title  "page $p$")
  (org-noter-auto-save-last-location t)
  (org-noter-kill-frame-at-session-end nil)
  (org-noter-always-create-frame nil)
  (org-noter-insert-selected-text-inside-note t)
  :init

  (defun ii/org-noter-read-book ()
    (let ((cands (calibredb-candidates)))
      (calibredb-get-file-path (consult--read cands
                                              :prompt "Pick a book"
                                              :lookup #'consult--lookup-cdr
                                              :sort nil))))

  (defun ii/org-noter-init-from-org ()
    (if-let* ((doc-file (ii/org-noter-read-book)))
        (progn
          (message "doc file is %s" doc-file)
          (org-set-property org-noter-property-doc-file doc-file)
          (org-noter nil))
      (user-error "Couldn't initialize org-noter session")))

  (defun ii/org-noter-start-session ()
    (interactive)
    (cond ((eq major-mode 'org-mode) (ii/org-noter-init-from-org))
          ((member major-mode '(pdf-view-mode djvu-mode nov-mode doc-view-mode))
           (org-noter))
          (t (user-error "This command can be used only in `org-mode', `pdf-view-mode', `nov-mode', `djvu-mode', `doc-view-mode'"))))
  :bind
  ( :map org-mode-map
    ("C-c nn" . ii/org-noter-start-session)
    :map pdf-view-mode-map
    ("C-c nn" . org-noter)
    :map nov-mode-map
    ("C-c nn" . org-noter)
    :map djvu-read-mode-map
    ("C-c nn" . org-noter)
    :map org-noter-doc-mode-map
    ("i" . org-noter-insert-note)
    ("i" . org-noter-insert-precise-note)
    ("C-c nq" . org-noter-kill-session)
    :map org-noter-notes-mode-map
    ("C-c nq" . org-noter-kill-session)))

;;TODO: replace pdf tools with emacs reader

;; (use-package emacs-reader
;;   )

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
         (not (bolp))
         meow-insert-mode
         (looking-at-p "\s*$")))
  :bind
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

(use-package gptel
  :config
  (setq gptel-model 'kwaipilot/kat-coder-pro:free
        gptel-include-reasoning nil
        ii/gptel-deepseek (gptel-make-deepseek "Deepseek"
                            :host "api.deepseek.com"
                            :stream t
                            :key #'gptel-api-key)
        ii/gptel-openrouter (gptel-make-openai "OpenRouter"
                              :host "openrouter.ai"
                              :endpoint "/api/v1/chat/completions"
                              :stream t
                              :key #'gptel-api-key
                              :models '(moonshotai/kimi-dev-72b:free
                                        kwaipilot/kat-coder-pro:free
                                        deepseek/deepseek-r1-0528-qwen3-8b:free))
        gptel-backend ii/gptel-deepseek)
  :hook
  (gptel-mode-hook . visual-line-mode)
  (gptel-post-stream-hook . gptel-auto-scroll)
  (gptel-post-response-function . gptel-end-of-response)
  :bind
  (("C-c ag" . gptel-menu)
   ("C-c aG" . gptel)
   ("C-c ar" . gptel-rewrite)
   :map embark-region-map
   :package embark
   ("R" . gptel-rewrite)))

(use-package gptel-agent
  :after (gptel))

(use-package agent-shell
  :init
  (setq agent-shell-header-style 'graphical
        agent-shell-file-completion-enabled t
        agent-shell-show-welcome-message nil
        agent-shell-highlight-blocks t)

  (defun ii/agent-shell-project (arg)
    (interactive "p")
    (ii/with-project-root! _
      (agent-shell arg)))

  :config
  (add-to-list 'whitespace-global-modes 'agent-shell-mode t)
  :bind
  (("C-c as" . agent-shell)
   ("C-c aS" . agent-shell-toggle)
   :map project-prefix-map
   ("a" . ii/agent-shell-project)))

;;Media
(setq max-image-size 15.0
      image-animate-loop t
      image-use-external-converter t
      image-auto-resize 'fit-window)

(with-eval-after-load 'image-mode

  (defvar ii/image-animate-max-delay 4)

  (defun ii/image-animate-timeout (image n count time-elapsed limit target-time)
    "Advice to allow for longer delay while displaying animation frames."
    (plist-put (cdr image) :animate-tardiness
               (+ (* (plist-get (cdr image) :animate-tardiness) 0.9)
                  (float-time (time-since target-time))))
    (let* ((buffer (plist-get (cdr image) :animate-buffer))
           (position (plist-get (cdr image) :animate-position))
           (continue-animation
            (and (buffer-live-p buffer)
                 ;; If we have a :animate-position setting, the caller
                 ;; has requested that the animation be stopped if the
                 ;; image is no longer displayed in the buffer.
                 (or (null position)
                     (with-current-buffer buffer
                       (let ((disp (get-text-property position 'display)))
                         (and (consp disp)
                              (eq (car disp) 'image)
                              ;; We can't check `eq'-ness of the image
                              ;; itself, since that may change.
                              (eq position
                                  (plist-get (cdr disp) :animate-position))))))
                 ;; Cumulatively delayed two seconds more than expected.
                 (or (< (plist-get (cdr image) :animate-tardiness) ii/image-animate-max-delay)
                     (progn
                       (message "Stopping animation; animation possibly too big")
                       nil)))))
      (if (not continue-animation)
          ;; Eject from the animation cache since we've decided not to
          ;; keep updating it.  This helps stop unbounded RAM usage when
          ;; doing, for instance, `g' in an eww buffer with animated
          ;; images.
          (clear-image-cache nil image)
        (let* ((time (prog1 (current-time)
                       (image-show-frame image n t)))
               (speed (image-animate-get-speed image))
               (time-to-load-image (time-since time))
               (stated-delay-time
                (/ (or (cdr (plist-get (cdr image) :animate-multi-frame-data))
                       image-default-frame-delay)
                   (float (abs speed))))
               ;; Subtract off the time we took to load the image from the
               ;; stated delay time.
               (delay (max (float-time (time-subtract stated-delay-time
                                                      time-to-load-image))
                           image-minimum-frame-delay))
               done)
          (setq n (if (< speed 0)
                      (1- n)
                    (1+ n)))
          (if limit
              (cond ((>= n count) (setq n 0))
                    ((< n 0) (setq n (1- count))))
            (and (or (>= n count) (< n 0)) (setq done t)))
          (setq time-elapsed (+ delay time-elapsed))
          (if (numberp limit)
              (setq done (>= time-elapsed limit)))
          (unless done
            (run-with-timer delay nil #'image-animate-timeout
                            image n count time-elapsed limit
                            (+ (float-time) delay)))))))

  (advice-add 'image-animate-timeout :override 'ii/image-animate-timeout)

  (bind-keys* :map image-mode-map
              ("<mouse-1>" . +utils/open-random-file-in-dir)
              ("r" . +utils/open-random-file-in-dir)))

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
(add-to-list 'auto-mode-alist '("\\(?:\\.\\(?:p\\(?:th\\|y[iw]?\\)\\)\\|/\\(?:SCons\\(?:\\(?:crip\\|truc\\)t\\)\\)\\)\\'" . python-ts-mode))
(add-to-list 'auto-mode-alist '("\\/git-rebase-todo\\'" . conf-mode))

(add-hook 'python-mode-hook #'python-ts-mode)

(use-package tide
  :init
  (setq tide-enable-xref t
        tide-imenu-flatten t
        tide-completion-detailed t)

  (ii/defhook! ii/tide--setup ()
    "Setup tide."
    :hook-var typescript-ts-mode-hook
    (tide-setup)
    (tide-hl-identifier-mode +1)))

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

(use-package haskell-mode
  :mode "\\.hs\\|.lhs\\'"
  :init
  (setq haskell-process-show-overlays t
        haskell-doc-show-prelude t
        haskell-doc-show-global-types t
        haskell-doc-show-user-defined t
        haskell-interactive-popup-errors nil))

(use-package haskell-ts-mode
  :custom
  (haskell-ts-font-lock-level 3)
  (haskell-ts-use-indent t)
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
    ("C-c C-l" . elisp-enable-lexical-binding)
    ("C-c C-j" . eval-print-last-sexp)
    ("C-c C-m" . emacs-lisp-macroexpand)))

(use-package eros
  :after (elisp-mode)
  :bind*
  ( :map emacs-lisp-mode-map
    ([remap eval-last-sexp] . eros-eval-last-sexp)
    ([remap eval-defun] . eros-eval-defun)
    ("C-c M-i" . eros-inspect-last-result)))

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

(use-package typst-preview
  :straight (typst-preview :type git
                           :host github
                           :repo "havarddj/typst-preview.el"))

(use-package typst-ts-mode
  :straight '(:type git :host codeberg :repo "meow_king/typst-ts-mode")
  :mode ("\\.typ\\'" . typst-ts-mode))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package nwscript-mode
  ;; :straight nil
  ;; :load-path "/home/b/programming/nwscript-mode.el/"
  :straight (nwscript-mode :type git
                           :host github
                           :repo "implicit-image/nwscript-mode.el")
  :autoload (nwscript-mode)
  :mode "\\.nss\\'"
  :init
  (setopt nwscript-compiler-executable (substitute-in-file-name "$HOME/.local/bin/nwnsc.exe"))
  :config
  ;; configure tempel templates
  ;; add base game includes
  (let ((file (substitute-in-file-name "$HOME/nwn2mods/game-includes")))
    (if (file-directory-p file)
        (add-to-list 'nwscript-include-dirs file))))

(use-package csv-mode
  :init
  (defun 2da-previous-field (n)
    (interactive "p")
    (2da-next-field (- n)))

  (defun 2da-next-nonempty-row (arg)
    (interactive "p")
    (let ((pos (point))
          (regexp "^[0-9]+[\t ]+[\"A-Za-z_-0-9]"))
      (save-mark-and-excursion
        (if (< 0 arg)
            (progn
              (forward-line 1)
              (search-forward-regexp regexp (point-max) t)
              (goto-char (match-beginning 0))
              (setq pos (pos-bol)))
          (search-backward-regexp regexp (point-min) t)
          (goto-char (match-end 0))
          (setq pos (pos-bol))))
      (when pos (goto-char pos))))

  (defun 2da-next-field (n)
    (interactive "p")
    (dotimes (_ (abs n))
      (2da--forward-field-1 (> n 0))))

  (defun 2da--forward-field-1 (forward)
    (cond (forward
           (cond ((eolp) (forward-line))
                 (t (when (thing-at-point 'symbol)
                      (forward-symbol 1))
                    (forward-whitespace 1))))
          (t
           (cond ((bolp) (forward-symbol -1))
                 (t (when (thing-at-point 'symbol)
                      (forward-symbol -1)))))))

  (defvar-local 2da--columns nil)

  (defun 2da-set-header-line ()
    "Set header line for 2da table."
    (interactive)
    (save-mark-and-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        ;; find first numbered line
        (search-forward-regexp "^[0-9]+[ \t]+.*" (point-max) t 1)
        (goto-char (match-beginning 0))
        (forward-line -1)
        (setq 2da--columns (append (list "ID") (string-split (thing-at-point 'line) '(32 ?\t))))
        (csv-header-line t))))

  (define-derived-mode 2da-mode csv-mode "2da"
    "Major mode for editing Bioware's 2da files."
    :interactive t
    (when font-lock-mode (font-lock-mode -1))
    (csv-align-mode 1)
    (setq-local csv-separators '("\t" "")
                csv-separator-chars '(?\t 32)
                buffer-invisibility-spec nil
                font-lock-keywords nil
                hscroll-margin 20
                csv-separator-regexp "[\t ]+"
                csv-font-lock-keywords nil
                csv-field-quotes nil)
    (2da-set-header-line))

  (bind-keys :map 2da-mode-map
             ("TAB" . 2da-next-field)
             ("C-<tab>" . 2da-previous-field)
             ("C-TAB" . 2da-previous-field))

  (add-to-list 'auto-mode-alist '("\\.2da\\'" . 2da-mode))
  :bind
  ( :map 2da-mode-map
    ("C-c C-l" . csv-header-line)))

(use-package crystal-mode
  :init
  (setq crystal-indent-level 4))

(use-package zig-ts-mode
  :straight (:type git :host codeberg :repo "meow_king/zig-ts-mode")
  :init
  (add-to-list 'auto-mode-alist '("\\.zig\\(?:\\.zon\\)?\\'" . zig-ts-mode)))

(use-package speed-type
  :init
  (setq speed-type-save-statistic-option 'always)
  :bind
  (("C-c ott" . speed-type-text)
   ("C-c otb" . speed-type-buffer)
   ("C-c otr" . speed-type-region)
   :map speed-type-mode-map
   ("C-c C-p" . speed-type-pause)))

(use-package elfeed
  :init
  (setq elfeed-db-directory (expand-file-name ".elfeed" user-emacs-directory)
        elfeed-enclosure-default-dir user-emacs-directory))

(use-package ement)

(use-package mastodon)

(use-package eask-mode)

(use-package osm
  :init
  (setq osm-max-tiles 512))

;; no config required
(ii/packages! f dash ov embark-consult verb devdocs vlf realgud ob-sql-mode djvu forge org-contrib htmlize
              ox-rss org-roam-ui poly-markdown poly-R poly-org fsharp-mode erlang cuda-mode nushell-mode
              xenops auctex ocaml-ts-mode dune reason-mode solidity-mode lean-mode d-mode gdscript-mode nim-mode
              gpr-mode idris-mode vhdl-ts-mode vhdl-ext nasm-mode masm-mode fasm-mode riscv-mode mips-mode fstar-mode
              sharper shader-mode sln-mode csproj-mode robe otp edts ess purescript-mode dart-mode common-lisp-snippets
              geiser racket-mode clj-refactor clojure-snippets cider clojure-mode groovy-mode sbt-mode pyvenv)

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

(+font--setup)

;;;; start profiler and profile the startup if emacs was started with --debug-init
(when init-file-debug
  (profiler-start 'cpu+mem)
  (message "starting profiler at %S" (current-time-string))
  (ii/defhook! ii/debug-stop-init-profiler ()
    "Stop and report profiler data after initialization."
    :hook-var after-init-hook
    :once t
    :depth 99
    (profiler-stop)
    (profiler-report)
    (message "finishing profiling at %S" (current-time-string))))

;;; init.el ends here;;
