;;; -*- lexical-binding: t -*-

(use-package lsp-mode
  :custom-face
  (lsp-lens-face ((t (:size ,+base/font-size))))
  (lsp-signature-highlight-function-argument ((t (:underline t))))
  :config
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
        lsp-modeline-diagnostics-enable nil
        ;;signature
        lsp-signature-auto-activate '(:on-trigger-char
                                      :on-server-request
                                      :after-completion)
        lsp-signature-render-documentation t
        lsp-signature-doc-lines 1
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
  :hook
  ((c-ts-mode-hook
    c++-ts-mode-hook
    java-ts-mode-hook
    kotlin-mode-hook
    python-ts-mode-hook
    typescript-ts-mode-hook
    rust-ts-mode-hook
    js-ts-mode-hook
    tsx-ts-mode-hook
    fstar-mode-hook
    nix-mode-hook)
   . lsp)
  :general
  (lsp-mode-map
   :states '(normal visual)
   "g r" 'lsp-find-references)
  (lsp-signature-mode-map
   :states '(insert emacs)
   "M-a" 'lsp-signature-toggle-full-docs)
  (lsp-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "t h" '("Headerline" . lsp-headerline-breadcrumb-mode)
   "c a" '("Apply code actions" . lsp-execute-code-action)
   "c D" '("Show doc buffer" . lsp-describe-thing-at-point)
   "c R" '("LSP rename" . lsp-rename)
   "c r" '("lsp find references" . lsp-find-references)))


(use-package lsp-ui
  :custom-face
  (lsp-ui-peek-footer ((t (:background ,(doom-color 'bg)))))
  (lsp-ui-peek-header ((t (:background ,(doom-color 'bg)))))
  (lsp-ui-doc-background ((t :background ,(doom-color 'base0))))
  :config
  (setq lsp-ui-peek-enable t
        lsp-ui-peek-always-show t
        lsp-ui-peek-list-width 40
        lsp-ui-peek-peek-height 15
        lsp-ui-peek-show-directory nil
        ;; sideline
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-update-mode 'point
        lsp-ui-sideline-diagnostic-max-line-length 200
        ;; docs
        lsp-ui-doc-enable nil
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-alignment 'window
        lsp-ui-doc-max-width 70
        lsp-ui-doc-max-height 10
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-delay nil
        lsp-ui-imenu-enable t
        lsp-ui-imenu-buffer-position 'right)
  :hook
  (lsp-mode-hook . (lambda ()
                     (interactive)
                     (lsp-ui-mode +1)
                     (lsp-ui-doc-mode +1)))
  :general
  (lsp-ui-mode-map
   :states '(normal visual insert)
   "C-c TAB" 'lsp-ui-doc-focus-frame)
  (lsp-ui-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "s i" '("LSP imenu" . lsp-ui-imenu)
   "c p r" '("Peek references" . lsp-ui-peek-find-references)
   "c p d" '("Peek definition" . lsp-ui-peek-find-definitions)
   "c p i" '("Peek implementation" . lsp-ui-peek-find-implementation)
   "c p s" '("Peek symbol" . lsp-ui-peek-find-workspace-symbol)
   "c d" '("Popup documentation" . lsp-ui-doc-show))
  (lsp-ui-peek-mode-map
   "j" 'lsp-ui-peek--select-next
   "k" 'lsp-ui-peek--select-prev))

(use-package consult-lsp
  :config
  (consult-customize
   consult-lsp-symbols
   consult-lsp-file-symbols
   consult-lsp-diagnostics :initial "")
  :general
  (lsp-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "c s" '("Local LSP symbols" . consult-lsp-file-symbols)
   "c S" '("Workspace LSP symbols" . consult-lsp-symbols)
   "c e" '("LSP diagnostics" . consult-lsp-diagnostics)))

(use-package lsp-treemacs
  :custom
  (lsp-treemacs-call-hierarchy-expand-depth t)
  (lsp-treemacs-error-list-expand-depth 3)
  (lsp-treemacs-type-hierarchy-epxand-depth 3)
  :hook
  (after-init . lsp-treemacs-sync-mode)
  :general
  (lsp-ui-mode-map
   :states '(normal visual)
   :prefix "SPC c"
   :global-prefix"C-c"
   "t e" '("Error list" . lsp-treemacs-errors-list)
   "t s" '("Symbols per file" . lsp-treemacs-symbols)
   "t r" '("References" . lsp-treemacs-references)
   "t i" '("Implementations" . lsp-treemacs-implementations)
   "t c" '("Call hierarchy" . lsp-treemacs-call-hierarchy)
   "t t" '("Type hierarchy" . lsp-treemacs-type-hierarchy)))
;; "T d" '("Dependencies" . lsp-treemacs-deps--icon)))

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

(use-package lsp-fsharp
  :straight nil)

(use-package lsp-nwscript
  :straight (lsp-nwscript :type git
                          :host github
                          :repo "implicit-image/lsp-nwscript.el"
                          :files ("lsp-nwscript.el")))

(provide 'init-lsp-mode)
