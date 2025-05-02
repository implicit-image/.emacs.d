

(+module/declare! ide)

(require 'init-format)

(require 'init-edit)

;;;; validating code and showing errors
(require 'init-checkers)

;;;; inserting and managing snippets
(require 'init-snippets)

(require 'init-indent)

;;;; debugging code
(require 'init-debug)

;;;; compiling and running code
(require 'init-compile)

;;;; in-buffer completion
;;;; custom LSP integration and utils
(require 'init-lsp)

;;;; standard elisp lsp client
;; (require 'init-lsp-mode)
(require 'init-cape)
(require 'init-corfu)
(require 'init-lsp-bridge)
(require 'init-lsp-mode)

(require 'init-ui)

(provide 'init-ide)
