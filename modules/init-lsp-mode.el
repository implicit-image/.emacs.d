;;; -*- lexical-binding: t -*-

(+when-idle! 3.0 (require 'lsp-mode))
(+when-idle! 4.0 (require 'lsp-ui))

(use-package lsp-mode
  :init
  (setq lsp-auto-configure t
        ;; lsp-mode features
        lsp-keymap-prefix "C-c l"
        lsp-enable-symbol-highlighting nil
        lsp-enable-xref t
        lsp-enable-imenu t
        lsp-enable-folding t
        lsp-enable-snippet t
        lsp-enable-dap-auto-configure t
        lsp-enable-file-watchers t
        lsp-enable-indentation t
        lsp-enable-on-type-formatting nil
        lsp-enable-symbol-highlighting t
        lsp-enable-suggest-server-download t
        lsp-enable-text-document-color t
        ;; modeline
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable t
        ;;signature
        lsp-signature-auto-activate '(:on-trigger-char
                                      :on-server-request
                                      :after-completion)
        lsp-signature-render-documentation t
        lsp-signature-doc-lines t
        lsp-signature-cycle t
        ;; completion
        lsp-completion-enable t
        lsp-completion-show-kind t
        lsp-completion-show-detail t
        lsp-completion-provider :none
        lsp-completion-default-behaviour :replace
        ;; headerline
        lsp-headerline-breadcrumb-enable nil
        lsp-headerline-breadcrumb-segments '(project path-up-to-project file symbols)
        lsp-headerline-breadcrumb-icons-enable nil
        ;; lenses
        lsp-lens-enable nil
        ;; eldoc
        lsp-eldoc-enable-hover t
        lsp-eldoc-render-all t
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
        lsp-typescript-auto-closing-tags t
        ;;nix
        lsp-nix-nixd-server-path (executable-find "nixd"))
  :bind*
  ( :map lsp-mode-map
    ("M-g r" . lsp-find-references)
    ("M-g d" . lsp-find-definition)
    ("M-g D" . lsp-find-declaration)
    ("M-g i" . lsp-find-implementation)
    ("M-g t" . lsp-find-type-definition)
    :map lsp-signature-mode-map
    ("M-a" . lsp-signature-toggle-full-docs)))


(use-package lsp-ui
  :config
  (setq lsp-ui-peek-enable t
        lsp-ui-peek-always-show t
        lsp-ui-peek-list-width 40
        lsp-ui-peek-peek-height 25
        lsp-ui-peek-show-directory t
        ;; sideline
        lsp-ui-sideline-enable nil
        ;; diagnostics
        lsp-diagnostics-provider :flymake
        ;; docs
        lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe nil
        lsp-ui-doc-alignment 'window
        lsp-ui-doc-max-width 60
        lsp-ui-doc-max-height 10
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay nil
        lsp-ui-imenu-enable t
        lsp-ui-imenu-buffer-position 'right)
  :bind*
  (:map lsp-command-map
        ("tf" . lsp-ui-doc-focus-frame)
        ("i" . lsp-ui-imenu)
        :map lsp-ui-mode-map
        ("M-g Pr" . lsp-ui-peek-find-references)
        ("M-g Pd" . lsp-ui-peek-find-definitions)
        ("M-g Pi" . lsp-ui-peek-find-implementation)
        ("M-g Ps" . lsp-ui-peek-workspace-symbol)
        ("M-g k" . lsp-ui-doc-show)
        :map lsp-ui-peek-mode-map
        ("j" . lsp-ui-peek--select-next)
        ("k" . lsp-ui-peek--select-prev)))

(use-package consult-lsp
  :config
  (consult-customize
   consult-lsp-symbols
   consult-lsp-file-symbols
   consult-lsp-diagnostics :initial "")
  :bind*
  ( :map lsp-command-map
    ("ss" . consult-lsp-file-symbols)
    ("sS" . consult-lsp-symbols)
    ("se" . consult-lsp-diagnostics)))

(use-package lsp-treemacs
  :custom
  (lsp-treemacs-call-hierarchy-expand-depth t)
  (lsp-treemacs-error-list-expand-depth 3)
  (lsp-treemacs-type-hierarchy-epxand-depth 3)
  :bind*
  ( :map lsp-command-map
    ("te" . lsp-treemacs-errors-list)
    ("ts" . lsp-treemacs-symbols)
    ("tr" . lsp-treemacs-references)
    ("ti" . lsp-treemacs-implementations)
    ("tc" . lsp-treemacs-call-hierarchy)
    ("tt" . lsp-treemacs-type-hierarchy)))

(use-package lsp-tailwindcss
  :straight (lsp-tailwindcss :type git
                             :host github
                             :repo "merrickluo/lsp-tailwindcss")
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

(use-package lsp-pyright
  :init
  (setq lsp-pyright-auto-import-completions t))

(use-package lsp-metals
  :after scala-mode)

(use-package lsp-java
  :init
  (setq lsp-java-references-code-lens-enabled t
        lsp-java-signature-help-enabled t
        lsp-java-signature-help-description-enabled t
        lsp-java-save-actions-organize-imports t
        lsp-java-completion-enabled t
        lsp-java-completion-overwrite t
        lsp-java-import-gradle-enabled t))

(use-package lsp-haskell
  :init
  (setq lsp-haskell-server-path "haskell-language-server-wrapper"
        ;; lsp-haskell-server-args '()
        lsp-haskell-check-project nil
        lsp-haskell-completion-in-comments nil
        lsp-haskell-plugin-eval-global-on nil
        lsp-haskell-plugin-semantic-tokens-global-on t))

(use-package lsp-nwscript
  :straight (lsp-nwscript :type git
                          :host github
                          :repo "implicit-image/lsp-nwscript.el"
                          :files ("lsp-nwscript.el")))

(provide 'init-lsp-mode)
