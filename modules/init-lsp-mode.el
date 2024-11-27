

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
  (setq lsp-enable-symbol-highlighting t
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
	lsp-signature-auto-activate t
	lsp-signature-render-documentation t
	lsp-signature-doc-lines 0
	;; completion
	lsp-completion-show-kind t
	lsp-completion-provider :none
	lsp-completion-enable t
	;; headerline
	lsp-headerline-breadcrumb-enable t
	lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols)
	lsp-headerline-breadcrumb-icons-enable t
	;; lenses
	lsp-lens-enable nil
	lsp-auto-configure t
	;; eldoc
	lsp-eldoc-enable-hover nil
	lsp-eldoc-render-all nil
	;; client config
	;; deno
	lsp-clients-deno-config "./tsconfig.json"
	lsp-clients-deno-enable-code-lens-implementations t
	lsp-clients-deno-enable-code-lens-references t
	lsp-clients-deno-enable-lint t
	;;php
	lsp-clients-php-server-command "phpactor -vv"
	;; rust-analyzer
	lsp-rust-analyzer-completion-auto-self-enable nil
	lsp-rust-analyzer-import-granularity 'module
	lsp-rust-analyzer-implicit-drops t
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
    haskell-mode
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
   . lsp)
  :general
  (lsp-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "t h" '("Headerline" . lsp-headerline-breadcrumb-mode)
   "t s" '("Sideline" . lsp-ui-sideline-mode)
   "c D" '("Show doc buffer" . lsp-describe-thing-at-point)
   "c R" '("LSP rename" . lsp-rename)
   "c r" '("lsp find references" . lsp-find-references)))

(use-package lsp-completion
  :no-require
  :straight nil
  :hook
  (lsp-mode . lsp-completion-mode))


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
  :config
  (+windows-cfg '((lsp-ui-imenu-mode)
		  :position bottom
		  :dedicated nil
		  :height 0.3))
  (setq lsp-ui-peek-enable t
	;; sideline
	lsp-ui-sideline-show-code-actions t
	lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-show-code-actions t
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
	lsp-ui-doc-position 'bottom
	lsp-ui-doc-delay 0.5
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
    "c p p" '("Peek implementation" . lsp-ui-peek-find-implementation)
    "c p d" '("Peek definition" . lsp-ui-peek-find-definitions)
    "c p i" '("Peek implementation" . lsp-ui-peek-find-implementation)
    "s i" '("LSP imenu" . lsp-ui-imenu))
  (lsp-ui-mode-map
   :states '(normal visual insert)
   "C-c TAB" 'lsp-ui-doc-focus-frame)
  (lsp-ui-doc-frame-mode
   :states '(normal visual insert)
   "<backtab>" 'lsp-ui-doc-unfocus-frame)
  (lsp-ui-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "t s" '("Toggle sideline display" . (lambda ()
					 (interactive)
					 (lsp-ui-sideline-mode)))
   "c a" '("Apply code actions" . lsp-ui-sideline-apply-code-actions)
   "c d" '("Popup documentation" . lsp-ui-doc-show)))


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
  :demand
  :init
  (setq corfu-cycle t
	corfu-doc-delay 0.00
	corfu-preselect 'first
	corfu-auto t
	corfu-popupinfo-delay '(0.4 . 0.2)
	corfu-left-margin-width 1
	corfu-right-margin-width 1
	corfu-bar-width 0
	corfu-count 15
	corfu-quit-no-match t
	global-corfu-minibuffer t)
  :config
  (global-corfu-mode +1)
  :custom-face
  (corfu-border ((t (:background ,(doom-color 'base5)))))
  (corfu-current ((t (:background
		      ,(doom-darken (doom-color 'base0) 0.2)
		      :foreground unspecified))))
  (corfu-default ((t (:background
		      ,(doom-color 'base1)
		      :foreground unspecified))))
  (corfu-popupinfo ((t (:box nil
			     :background ,(doom-color 'base1)))))
  (corfu-echo ((t (:foreground ,(doom-color 'fg-alt)))))
  :hook
  (corfu-mode . (lambda ()
		  (corfu-echo-mode +1)
		  (corfu-history-mode +1)
		  (corfu-popupinfo-mode +1)))
  (lsp-bridge . (lambda ()
		  (corfu-mode -1)))
  :general
  (corfu-map
   :states 'insert
   "M-h" 'corfu-doc-toggle
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
    :hook
    (tty-setup . corfu-terminal-mode))

  (use-package corfu-doc-terminal
    :after corfu-terminal
    :straight (corfu-doc-terminal :type git
				  :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git")
    :hook
    (tty-setup . corfu-doc-terminal-mode)))

(use-package corfu-candidate-overlay
  :after corfu
  :config
  (set-face-attribute 'corfu-candidate-overlay-face nil :inherit font-lock-comment-face)
  (corfu-candidate-overlay-mode +1))

(use-package cape
  :after corfu
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))


(provide 'init-lsp-mode)
