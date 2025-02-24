(require 'f)

(require 'init-indent)

(require 'init-format)

(require 'init-edit)

;;;; validating code and showing errors
(require 'init-checkers)

;;;; inserting and managing snippets
(require 'init-snippets)

;;;; debugging code
(require 'init-debug)

;;;; compiling and running code
(require 'init-compile)

;;;; in-buffer completion

(if (string-equal (system-name) "nixos")
    (require 'init-company)
  (require 'init-corfu))

;;;; custom LSP integration and utils
(require 'init-lsp)

;;;; standard elisp lsp client
(require 'init-lsp-mode)

;;;; fastest lsp client, uses external
;; (require 'init-lsp-bridge)

;;;; simple LSP client implemented as an emacs module.
;; (require 'init-lspce)

(require 'init-ui)

(provide 'init-ide)
