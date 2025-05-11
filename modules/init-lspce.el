;;; -*- lexical-binding: t -*-

(use-package lspce
  :straight `(lspce :type git
                    :host github
                    :repo "zbelial/lspce"
                    :files (:defaults
                            ,(pcase system-type
                               ('gnu/linux "lspce-module.so")
                               ('darwin "lspce-module.dylib")))
                    :pre-build ,(pcase system-type
                                  ('gnu/linux '(("cargo" "build" "--release") ("cp" "./target/release/liblspce_module.so" "./lspce-module.so")))
                                  ('darwin '(("cargo" "build" "--release") ("cp" "./target/release/liblspce_module.dylib" "./lspce-module.dylib")))))
  :init
  (defun +lspce--type-function ()
    "Figure out lsp id of the buffer."
    (let ((parsed (string-trim-right (symbol-name major-mode) "-\\(ts-\\)?\\(mode\\)")))
      ;; translation
      (pcase parsed
        ("anaconda" "python")
        ("rjsx" "js")
        ("literate-haskell" "haskell")
        ("haskell-literate" "haskell")
        (_ parsed))))

  (defun +lspce-setup ()
    "Setuo lspce local variables."
    (add-to-list 'xref-backend-functions 'lspce-xref-backend)
    (lspce-inlay-hints-mode 1))
  :config
  (setq lspce-lsp-type-function #'+lspce--type-function)
  (setq lspce-server-programs '(("rust" "rust-analyzer" "")
                                ("python" "jedi-language-server" "")
                                ("python" "pylsp" "")
                                ("haskell" "haskell-language-server-wrapper" "lsp")
                                ("c" "ccls" "")
                                ("sh" "bash-language-server" "start")
                                ("go" "gopls" "")
                                ("typescript" "typescript-language-server" "--stdio")
                                ("js" "typescript-language-server" "--stdio")
                                ("java" "jdtls" "")))
  :hook
  (lspce-mode . +lspce-setup))
;; :general
;; (lspce-mode-map
;;  :states '(normal visual)
;;  :prefix "SPC"
;;  :non-normal-prefix "C-c"
;;  "c a" '("Code Axtions" . lspce-code-actions)
;;  "c s i" '("Incoming calls" . lspce-incoming-calls)
;;  "c s o" '("Outgoing calls" . lspce-outgoing-calls)
;;  "t i" '("Toggle inlay hints" . lspce-inlay-hints-mode)
;;  "c R" '("Rename symbol" . lspce-rename)))



(provide 'init-lspce)
