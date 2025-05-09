;;; -*- lexical-binding: t -*-


;;; Code:


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
        indent-bars-color-by-depth '(:regexp "outline-\\([0-9]+\\)" :blend 1)
        indent-bars-highlight-current-depth '(:blend 0.5)
        indent-bars-display-on-blank-lines 'least)
  :custom
  (indent-bars-prefer-character t)
  :hook
  ((c-mode-hook
    gleam-ts-mode-hook
    java-ts-mode-hook
    js-ts-mode-hook
    tsx-ts-mode-hook
    typescript-ts-mode-hook
    c-ts-mode-hook
    js2-mode-hook
    css-mode-hook
    nix-mode-hook
    rust-ts-mode-hook)
   . indent-bars-mode))

(use-package whitespace
  :custom-face
  (whitespace-space ((t (:foreground ,(doom-color 'base4)))))
  (whitespace-hspace ((t (:foreground ,(doom-color 'bg) :background ,(doom-color 'bg)))))
  (whitespace-indentation ((t (:foreground ,(doom-color 'base4)))))
  :init
  (setq whitespace-global-modes '(not markdown-mode gfm-mode org-mode latex-mode dired-mode csv-mode nxml-mode ess-mode diff-mode magit-mode magit-diff-mode))
  (defun +whitespace-toggle-style ()
    "Toggle whitespace mode display style."
    (if (display-graphic-p)
        (setq whitespace-style '(face spaces indentation::space space-mark))
      (setq whitespace-style '(face line spaces tabs indentation::space indentation::tab indentation big-indent))))

  (defun +whitespace-on ()
    (interactive)
    (whitespace-turn-on))

  (defun +whitespace-off ()
    (interactive)
    (whitespace-turn-off))

  :config
  ;; (setq-default whitespace-space-regexp "\\(^ +\\)")
  (setq whitespace-display-mappings '((space-mark 32 ;; space
                                                  [183]
                                                  [46])
                                      (space-mark 160 ;; hard space
                                                  [164]
                                                  [95])
                                      (newline-mark 10
                                                    [36 10])
                                      (tab-mark 9
                                                [187 9]
                                                [92 9])))
  :hook
  (enable-theme-functions . global-whitespace-mode)
  (before-save-hook . whitespace-cleanup)
  (whitespace-mode-hook . +whitespace-toggle-style)
  (before-save-hook . delete-trailing-whitespace))

;; (general-def global-map
;;   :states 'insert
;;   "TAB" '+smart-tab
;;   "<tab>" '+smart-tab
;;   [tab] '+smart-tab)

(setq tab-always-indent 'complete
      tab-first-completion 'complete)



(provide 'init-indent)
