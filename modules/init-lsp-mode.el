;;; -*- lexical-binding: t -*-

(defvar +lsp/installers '((npm-global . "npm install -g %s"))
  "Alist of (SYMBOL . COMMAND-STRING) where COMMAND-STRING is a command to install a server.")

(defun +lsp--install-server (server)
  (format (alist-get server +lsp/installers) ))

(defun +lsp/scroll-up-doc-popup (&optional count)
  ""
  (interactive)
  (lsp-ui-doc-focus-frame)
  (evil-scroll-up count)
  (lsp-ui-doc-unfocus-frame))

(defun +lsp/install-servers (&optional force)
  "Install all servers in `+lsp/servers-to-install'")

(defun +lsp/counsel-code-actions ()
  (interactive)
  (ivy-read "Execute code action: "
	    (mapcar (lambda (action)
		      (plist-get action :title))
		    lsp-ui-sideline--code-actions)
	    :keymap counsel-describe-map
	    :require-match t
	    :caller '+lsp/counsel-ccode-actions
	    :action #'lsp-execute-code-action))


(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))

(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)


(use-package lsp-mode
  :config
  (+windows-cfg
   '(("\*lsp-help\**" "\*lsp-install*")
     :regexp t :height 0.35 :position bottom :dedicated nil))
  (setq lsp-auto-configure t
	;; lsp-mode features
	lsp-enable-symbol-highlighting t
	lsp-enable-xref t
	lsp-enable-imenu t
	lsp-enable-folding t
	lsp-enable-dap-auto-configure t
	lsp-enable-file-watchers t
	lsp-enable-indentation t
	lsp-enable-on-type-formatting nil
	lsp-enable-symbol-highlighting t
	lsp-enable-suggest-server-download t
	;; modeline
	lsp-modeline-code-actions-enable nil
	lsp-modeline-diagnostics-enable nil
	;;signature
	lsp-signature-auto-activate '(:on-trigger-char
				      :on-server-request
				      :after-completion)
	lsp-signature-render-documentation nil
	lsp-signature-doc-lines nil
	lsp-signature-cycle t
	;; completion
	lsp-completion-show-kind nil
	lsp-completion-show-detail nil
	lsp-completion-provider :none
	lsp-completion-enable t
	lsp-completion-show-label-description nil
	lsp-completion-default-behaviour :replace
	;; headerline
	lsp-headerline-breadcrumb-enable nil
	lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols)
	lsp-headerline-breadcrumb-icons-enable nil
	;; lenses
	lsp-lens-enable nil
	;; eldoc
	lsp-eldoc-enable-hover nil
	lsp-eldoc-render-all nil
	;; deno
	lsp-clients-deno-config "./tsconfig.json"
	lsp-clients-deno-enable-code-lens-implementations t
	lsp-clients-deno-enable-code-lens-references t
	lsp-clients-deno-enable-lint t
	;;php
	lsp-clients-php-server-command "phpactor -vv"
	lsp-rust-server "rust-analyzer"
	;; rust-analyzer
	lsp-rust-analyzer-implicit-drops t
	lsp-rust-analyzer-completion-auto-self-enable nil
	;; typescript
	lsp-typescript-suggest-auto-imports t
	lsp-typescript-auto-closing-tags t)
  :hook
  ;; default lsp-mode setup
  ((lsp-help-mode) . (lambda ()
		       (interactive)
		       (display-line-numbers-mode -1)))
  ((rust-ts-mode
    gleam-ts-mode
    idris-mode
    tsx-ts-mode
    ocaml-ts-mode
    css-ts-mode
    css-mode
    typescript-ts-mode
    js-ts-mode
    c-ts-mode
    c++-ts-mode
    web-mode
    rjsx-mode)
   . lsp-deferred)
  :general
  (lsp-mode-map
   :states '(normal visual)
   "g r" 'lsp-find-references)
  (lsp-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "t h" '("Headerline" . lsp-headerline-breadcrumb-mode)
   "c D" '("Show doc buffer" . lsp-describe-thing-at-point)
   "c R" '("LSP rename" . lsp-rename)
   "c r" '("lsp find references" . lsp-find-references)))


(use-package lsp-tailwindcss
  :straight (lsp-tailwindcss :type git
			     :host github
			     :repo "merrickluo/lsp-tailwindcss")
  :after lsp-mode
  :init
  (setq lsp-tailwindcss-addon-mode t)
  :config
  (dolist (tw-major-mode
	   '(css-mode
	     css-ts-mode
	     typescript-mode
	     typescript-ts-mode
	     tsx-ts-mode
	     js2-mode
	     js-ts-mode
	     clojure-mode))
    (add-to-list 'lsp-tailwindcss-major-modes tw-major-mode)))

(use-package lsp-ui
  :custom-face
  (child-frame-border ((t (:background ,(doom-color 'fg)))))
  (lsp-ui-peek-header ((t (:background ,(doom-color 'base6)))))
  (lsp-ui-peek-footer ((t (:background ,(doom-color 'base6)))))
  (lsp-ui-peek-peek ((t (:background ,(doom-lighten (doom-color 'bg) 0.1)))))
  (lsp-ui-peek-list ((t (:background ,(doom-lighten (doom-color 'bg) 0.1)))))
  :config
  (+windows-cfg '((lsp-ui-imenu-mode)
		  :position bottom
		  :dedicated nil
		  :height 0.3))
  (setq lsp-ui-peek-enable t
	lsp-ui-peek-always-show t
	lsp-ui-peek-list-width 40
	lsp-ui-peek-peek-height 15
	lsp-ui-peek-show-directory nil
	;; sideline
	lsp-ui-sideline-show-code-actions t
	lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-show-code-actions t
	lsp-ui-sideline-ignore-duplicate t
	lsp-ui-sideline-enable t
	;; docs
	lsp-ui-doc-enable t
	lsp-ui-doc-use-childframe nil
	lsp-ui-doc-alignment 'frame
	lsp-ui-doc-max-width 20
	lsp-ui-doc-max-height 20
	lsp-ui-doc-header nil
	lsp-ui-doc-max-height 20
	lsp-ui-doc-max-width 30
	lsp-ui-doc-include-signature t
	lsp-ui-doc-show-with-cursor t
	lsp-ui-doc-show-with-mouse t
	lsp-ui-doc-position 'at-point
	lsp-ui-doc-delay 0.1
	lsp-ui-imenu-enable t
	lsp-ui-imenu-buffer-position 'right)
  :hook
  (lsp-mode . (lambda ()
		(interactive)
		(lsp-ui-mode +1)
		(lsp-ui-doc-mode +1)
		(lsp-ui-sideline-mode +1)))
  (lsp-ui-imenu-mode . (lambda ()
			 (display-line-numbers-mode -1)))
  :general
  (+leader-keys
    "t s" '("Sideline" . lsp-ui-sideline-mode))
  (lsp-ui-mode-map
   :states '(normal visual insert)
   "C-c TAB" 'lsp-ui-doc-focus-frame)
  (lsp-ui-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "t s" '("Toggle sideline display" . (lambda ()
					 (interactive)
					 (lsp-ui-sideline-mode)))
   "s i" '("LSP imenu" . lsp-ui-imenu)
   "c a" '("Apply code actions" . lsp-ui-sideline-apply-code-actions)
   "c p r" '("Peek references" . lsp-ui-peek-find-references)
   "c p d" '("Peek definition" . lsp-ui-peek-find-definitions)
   "c p i" '("Peek implementation" . lsp-ui-peek-find-implementation)
   "c p s" '("Peek symbol" . lsp-ui-peek-find-workspace-symbol)
   "c d" '("Popup documentation" . lsp-ui-doc-show))
  (lsp-ui-peek-mode-map
   "j" 'lsp-ui-peek--select-next
   "k" 'lsp-ui-peek--select-prev))


(use-package lsp-ivy
  :commands
  (lsp-ivy-workspace-symbol lsp-ivy-global-workspace-symbol)
  :general
  (lsp-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "c s" '("Search LSP symbol" . lsp-ivy-workspace-symbol)
   "c S" '("Search LSP symbol" . lsp-ivy-global-workspace-symbol)))


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

(provide 'init-lsp-mode)
