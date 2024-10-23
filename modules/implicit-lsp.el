
(use-package lsp-mode
  :config
  (push '("\*lsp-help\**"
	  :regexp t
	  :height 0.35
	  :position bottom
	  :dedicated nil) popwin:special-display-config)
  (setq lsp-enable-symbol-highlighting t
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-signature-auto-activate t
        lsp-signature-render-documentation nil
        lsp-completion-show-detail t
        lsp-completion-show-kind t
	lsp-completion-provider :capf
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-enable-diagnostics t
        lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols)
        lsp-headerline-breadcrumb-icons-enable t
        lsp-lens-enable nil
	lsp-diagnostics-provider 'auto
	lsp-eldoc-enable-hover t)
  :hook
  ;; modes to autostart lsp in
  ((c-ts-mode
    c-mode
    c++-ts-mode
    c++-mode
    rust-mode
    rust-ts-mode
    python-ts-mode
    python-mode
    haskell-mode
    literate-haskell-mode) . (lambda ()
	  (interactive)
	  (lsp-deferred))))

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-show-code-actions t
	lsp-ui-sideline-show-diagnostics t
	lsp-ui-sideline-show-hover t
	lsp-ui-sideline-show-code-actions t
	lsp-ui-sideline-enable t
	lsp-ui-doc-enable t
	lsp-ui-peek-enable t
	lsp-ui-doc-alignment 'at-point
	lsp-ui-doc-include-signature t
	lsp-ui-doc-show-with-cursor t
	lsp-ui-doc-show-with-mouse nil
	lsp-ui-doc-position 'top
	lsp-ui-doc-delay 0.0))




;; lsp booster functionality
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


(provide 'implicit-lsp)
