;;; -*- lexical-binding: t -*-

(defvar +lsp/servers-to-install '()
  "Alist of (INSTALLER-SYMBOL . SERVER-NAME) to ensure are installed by `lsp-install-server'")

;;;###autoload
(defun +lsp/init-choose-client ()
  (interactive)
  (consult--read '("lsp-mode" "lsp-bridge" "eglot" "none")
	    :prompt "LSP client: "
	    :require-match t
	    :history '+lsp/choose-client-history
	    :lookup (lambda (client)
		      (interactive)
		      (pcase client
			("lsp-mode" ((lsp-deferred)))
			("eglot" ((lsp-deferred)))
			("lsp-bridge" ((lsp-bridge-mode +1)))
			("none" ((ignore)))))))


(provide 'init-lsp)
