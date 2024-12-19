;;; -*- lexical-binding: t -*-

(use-package indent-bars
  :straight (indent-bars :type git
			 :host github
			 :repo "jdtsmith/indent-bars")
  :commands
  indent-bars-mode
  :config
  (setq indent-bars-color '(highlight :face-bg t :blend 0.15)
	indent-bars-starting-column 0
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
  ((c-mode gleam-ts-mode java-ts-mode js2-ts-mode nwscript-mode tsx-ts-mode c-ts-mode js2-mode css-mode rust-ts-mode) . indent-bars-mode))

(use-package whitespace
  :custom-face
  (whitespace-space ((t (:foreground "#4a4a4a"))))
  (whitespace-empty ((t (:foreground "#4a4a4a"))))
  :config
  (setq whitespace-style '(face indentation tabs spaces tab-mark space-mark)
	whitespace-display-mappings ((space-mark 32
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
  ((prog-mode) . whitespace-mode)
  (before-save . whitespace-cleanup)
  (before-save . delete-trailing-whitespace))

(provide 'init-indent)
