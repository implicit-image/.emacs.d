;;; -*- lexical-binding: t -*-

(use-package corfu
  :custom-face
  (corfu-border ((t (:background ,(doom-color 'base5)))))
  (corfu-current ((t (:background
		      ,(doom-darken (doom-color 'base4) 0.2)
		      :foreground unspecified))))
  (corfu-default ((t (:background
		      ,(doom-color 'base0)
		      :foreground unspecified))))
  (corfu-popupinfo ((t (:box nil
			     :background ,(doom-color 'base1)))))
  (corfu-echo ((t (:foreground ,(doom-color 'fg-alt)))))
  :init
  (setq corfu-cycle t
	corfu-doc-delay 0.0
	corfu-echo-delay '(0.3 . 0.15)
	corfu-auto-delay 0.0
	corfu-preselect 'first
	corfu-auto nil
	corfu-popupinfo-delay '(0.3 . 0.15)
	corfu-left-margin-width 3
	corfu-right-margin-width 1
	corfu-bar-width 0
	corfu-count 15
	corfu-max-width 50
	corfu-quit-no-match t
	corfu-on-exact-match 'show
	global-corfu-minibuffer nil)
  :hook
  (corfu-mode . (lambda ()
		  (corfu-echo-mode +1)
		  (corfu-history-mode +1)
		  (corfu-popupinfo-mode +1)))
  (lsp-bridge . (lambda ()
		  (corfu-mode -1)))
  (after-init . global-corfu-mode)
  :general
  (corfu-map
   :states 'insert
   "M-h" 'corfu-popupinfo-toggle
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
    :init
    (setq corfu-terminal-enable-on-minibuffer nil
	  corfu-terminal-disable-on-gui t
	  corfu-terminal-position-right-margin 5)
    :hook
    (tty-setup . corfu-terminal-mode))

  (use-package corfu-doc-terminal
    :after corfu-terminal
    :straight (corfu-doc-terminal :type git
				  :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
    :hook
    (tty-setup . corfu-doc-terminal-mode)))

(use-package corfu-candidate-overlay
  :custom-face
  (corfu-candidate-overlay-face ((t (:foreground ,(doom-color 'doc-comments)))))
  :hook
  (corfu-mode . (lambda ()
		 (interactive)
		 (corfu-candidate-overlay-mode +1)))
  :general
  (corfu-mode-map
   "C-RET" 'corfu-candidate-overlay-complete-at-point
   "C-<return>" 'corfu-candidate-overlay-complete-at-point))

(use-package cape
  :after corfu
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(provide 'init-corfu)
