;;; -*- lexical-binding: t -*-
(setq meow-leader-global-map (make-sparse-keymap)
      meow-llm-global-map (make-sparse-keymap)
      meow-special-global-map (make-sparse-keymap)
      meow-jump-global-map (make-sparse-keymap)
      meow-notes-global-map (make-sparse-keymap)
      meow-toggle-global-map (make-sparse-keymap)
      meow-mc-global-map (make-sparse-keymap)
      meow-vc-global-map (make-sparse-keymap)
      flymake-prefix-map (make-sparse-keymap)
      meow-insert-global-map (make-sparse-keymap)
      meow-search-global-map (make-sparse-keymap)
      meow-file-global-map (make-sparse-keymap)
      meow-grep-global-map (make-sparse-keymap)
      meow-buffer-global-map (make-sparse-keymap))


;;; Code:
(use-package emacs
  :demand
  :custom
  ;;;; built-in global options.
  (user-full-name "Błażej Niewiadomski")
  (user-mail-address "blaz.nie@protonmail.com") ;; raised to allow better lsp speeds)
  (visible-bell nil)
  (ring-bell-function 'ignore)
  (display-line-numbers-type 'relative)
  (truncate-lines 40)
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil)
  (truncate-partial-width-windows t)
  (create-lockfiles nil)
  (x-stretch-cursor nil)
  (backup-inhibited t)
  (make-backup-files nil)
  (enable-recursive-minibuffers t)
  (scroll-step 1)
  (find-file-wildcards nil)
  (comment-multi-line t)
  (read-extended-command-predicate 'command-completion-default-include-p)
  (comment-empty-lines t)
  (lazy-highlight-initial-delay 0)
  (completion-ignore-case t)
  (sentence-end-double-space nil)
  (use-dialog-box nil)
  (use-file-dialog nil)
  (use-short-answers t)
  (read-answer-short t)
  (left-fringe-width 5)
  (right-fringe-width 5)
  (indicate-buffer-boundaries nil)
  (indicate-empty-lines nil)
  (word-wrap t)
  (indent-tabs-mode nil)
  (tab-width 4)
  (delete-selection-save-to-register t)
  (next-screen-context-lines 0)
  (auto-window-vscroll nil)
  ;; (scroll-conservatively )
  (scroll-preserve-screen-position t)
  (scroll-error-top-bottom t)
  (save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-limit 600)
  (tab-always-indent 'complete)
  (tab-first-completion 'complete)
  :config
  ;; insert matching parens
  (electric-pair-mode 1)
  ;; highlight current line
  (global-hl-line-mode 1)
  (repeat-mode 1)
  (savehist-mode 1)
  (save-place-mode 1)
  (context-menu-mode 1)
  (blink-cursor-mode -1)
  ;; write customizations to seperate file
  (let ((customization-file
         (expand-file-name "custom.el" user-emacs-directory)))
    (unless (file-exists-p customization-file)
      (write-region "" nil customization-file))
    (setq custom-file customization-file)
    (load custom-file 'noerror))
  ;; setup fonts
  (set-frame-font +base/font-spec nil t t)
  (set-frame-font +base/font-spec)
  (set-face-attribute 'default t
                      :font +base/font-spec)
  ;; add an option to diff current buffer with its file on disk
  (add-to-list 'save-some-buffers-action-alist
               (list "d"
                     (lambda (buffer)
                       (diff-buffer-with-file (buffer-file-name buffer)))
                     "show diff between the buffer and its file"))
  :bind
  (("M-RET" . recenter)
   ("M-<return>" . recenter)
   ("M-]" . forward-paragraph)
   ("M-[" . backward-paragraph))
  :hook
  ;; truncate lines in modes derived from prog-mode
  ;; better to see all of the code
  (prog-mode-hook . visual-wrap-prefix-mode)
  ((help-mode-hook helpful-mode-hook lsp-ui-doc-hook) . visual-line-mode)
  (tty-setup . +tty-setup)
  ;; display line numbers in text-editing modes
  ((prog-mode-hook
    markdown-ts-mode-hook
    org-mode-hook
    latex-mode-hook
    org-roam-mode-hook
    gfm-mode-hook
    text-mode-hook
    conf-mode-hook
    tuareg-mode-hook)
   . display-line-numbers-mode)
  (after-init-hook . (lambda () (message (emacs-init-time)))))

(use-package init-utils
  :straight nil
  :bind*
  (("M-<backspace>" . backward-kill-word)
   :map meow-file-global-map
   ("C" . +utils/copy-visited-file)
   ("D" . +utils/delete-visited-file)
   ("P" . +utils/browse-modules)
   ("p" . +utils/ripgrep-user-directory)
   ("R" . +utils/rename-visited-file)
   ("y" . +utils/yank-current-file)
   ("Y" . +utils/yank-current-path)
   :map meow-toggle-global-map
   ("f" . +utils/consult-set-font-family)
   :map meow-insert-global-map
   ("!" . +utils/insert-shell-command-output)))

;; load $PATH from shell
(use-package exec-path-from-shell
  :commands exec-path-from-shell-initialize
  :functions exec-path-from-shell-getenv
  :if (+os/is-linux-p)
  :init
  (setq exec-path-from-shell-variables '("PATH" "TERM" "MANPATH" "JAVA_HOME")
        exec-path-from-shell-shell-name (cond ((or (+os/is-wsl-p)
                                                   (+os/is-linux-p))
                                               "zsh")
                                              ((+os/is-windows-p) "powershell")))
  :hook
  (after-init-hook . exec-path-from-shell-initialize))

;; which key display
(use-package which-key
  :straight nil
  :commands
  (which-key-setup-side-window-right-bottom)
  :init
  (setq which-key-popup-type 'side-window
        which-key-preserve-window-configuration t
        which-key-side-window-max-width 0.2
        which-key-idle-delay 0.8
        which-key-idle-secondary-delay 0.05
        which-key-separator " -> "
        which-key-max-display-columns 5
        which-key-add-column-padding 5
        which-key-show-remaining-keys t
        which-key-min-column-description-width 30)
  ;; display `which-key' window on bottom side of the frame
  (which-key-setup-side-window-right-bottom)
  :hook
  ;; load after loading user init
  (after-init-hook . which-key-mode))

(use-package kkp
  :hook
  (tty-setup-hook . global-kkp-mode))

(provide 'init-base)
