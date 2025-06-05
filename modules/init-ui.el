;;; -*- lexical-binding: t -* t t t-


(use-package whitespace
  :init
  (setq whitespace-global-modes '(not markdown-mode gfm-mode org-mode latex-mode dired-mode csv-mode nxml-mode ess-mode diff-mode magit-mode magit-diff-mode)
        whitespace-display-mappings '((space-mark 32 ;; space
                                                  [183]
                                                  [46])
                                      (space-mark 160 ;; hard space
                                                  [164]
                                                  [95])
                                      (newline-mark 10
                                                    [36 10])
                                      (tab-mark 9
                                                [187 9]
                                                [92 9]))
        whitespace-style '(face spaces trailing lines-char newline space-mark tab-mark))

  :hook
  (before-save-hook . whitespace-cleanup)
  (before-save-hook . delete-trailing-whitespace))

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
          indent-bars-width-frac 0.1
          indent-bars-pad-frac 0.1
          indent-bars-zigzag nil
          indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
          indent-bars-highlight-current-depth '(:blend 0.5)
          indent-bars-display-on-blank-lines t
          indent-bars-spacing 4
          indent-bars-prefer-character t)
  :hook
  (prog-mode-hook
   . +indent-bars-mode--setup))

(use-package sideline-blame
  :init
  (setq sideline-blame-commit-format "( %s )"))

(use-package sideline-lsp
  :init
  (setq sideline-lsp-ignore-duplicate t
        sideline-lsp-update-mode 'line
        sideline-lsp-code-actions-prefix "[!]"))

(use-package sideline
  :init
  (setq sideline-backends-right `((sideline-lsp . up)
                                  sideline-blame)
        sideline-backend-delays '((sideline-blame . 0.1)
                                  (sideline-lsp . 1))
        sideline-backends-right-skip-current-line nil
        sideline-truncate t
        sideline-force-display-if-exceeds nil
        sideline-order-right 'up
        sideline-format-right "%s"
        sideline-display-backend-name nil
        sideline-display-backend-format ""
        sideline-display-backend-type nil)
  :bind
  ( :map meow-toggle-global-map
    ("s" . sideline-mode))
  :hook
  (prog-mode . sideline-mode))

(setopt which-func-display 'header
        which-func-modes '(prog-mode)
        which-func-update-delay 0.2
        which-func-format '("      "
                            (:propertize which-func-current
                                         face which-func mouse-face mode-line-highlight help-echo
                                         "Current function\nmouse-1: go to beginning\nmouse-2: toggle rest visibility\nmouse-3: go to end")
                            ""))

(add-hook 'after-init-hook 'which-function-mode)

(provide 'init-ui)
;;; init-ui.el ends here
