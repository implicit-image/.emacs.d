;;; -*- lexical-binding: t -*-


;;; Code:

(defvar +indent/tab-jump-delims '(?\; ?\) ?\] ?} ?> ?| ?' ?` ?\"))

(defun +smart-tab (&optional prefix)
  ""
  (interactive "P")
  (cond ((memq (char-after (point)) +indent/tab-jump-delims) (forward-char))
        (nil (forward))
        (t (indent-for-tab-command prefix))))

(use-package indent-bars
  :straight (indent-bars :type git
                         :host github
                         :repo "jdtsmith/indent-bars")
  :commands
  indent-bars-mode
  :config
  (setq indent-bars-color '(highlight :face-bg t :blend 0.15)
        indent-bars-starting-column nil
        indent-bars-pattern "."
        indent-bars-width-frac 0.1
        indent-bars-pad-frac 0.1
        indent-bars-zigzag nil
        indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1) ; blend=1: blend with BG only
        indent-bars-highlight-current-depth '(:blend 0.5) ; pump up the BG blend on current
        indent-bars-display-on-blank-lines t)
  :custom
  (indent-bars-prefer-character t)
  :hook
  ((c-mode
    gleam-ts-mode
    java-ts-mode
    js-ts-mode
    nwscript-mode
    tsx-ts-mode
    typescript-ts-mode
    c-ts-mode
    js2-mode
    css-mode
    nix-mode
    rust-ts-mode)
   . indent-bars-mode))

(use-package whitespace
  :custom-face
  (whitespace-space ((t (:foreground ,(doom-lighten (doom-color 'bg) 0.1)))))
  (whitespace-empty ((t (:foreground ,(doom-lighten (doom-color 'bg) 0.1)))))
  :init
  (defun +whitespace-toggle-style ()
    "Toggle whitespace mode display style."
    (if (display-graphic-p)
        (setq whitespace-style '(face indentation tabs spaces tab-mark space-mark))
      (setq whitespace-style '(face line spaces tabs))))

  (setq-default indent-tabs-mode nil)
  (setq whitespace-style '(face indentation tabs spaces tab-mark space-mark)
        whitespace-display-mappings '((space-mark 32
                                                  [183]
                                                  [46])
                                      (space-mark 160
                                                  [164]
                                                  [95])
                                      (newline-mark 10
                                                    [36 10])
                                      (tab-mark 9
                                                [187 9]
                                                [92 9])))
  :hook
  ((prog-mode emacs-lisp-mode nwscript-mode) . whitespace-mode)
  (before-save . whitespace-cleanup)
  (whitespace-mode . +whitespace-toggle-style)
  (before-save . delete-trailing-whitespace))

(general-def global-map
  :states 'insert
  "TAB" '+smart-tab
  "<tab>" '+smart-tab
  [tab] '+smart-tab)

(provide 'init-indent)
