
(defvar +lsp/servers-to-install '()
  "Alist of (INSTALLER-SYMBOL . SERVER-NAME) to ensure are installed by `lsp-install-server'")

(defun +lsp/doc-popup ())
(defun +lsp/doc-buffer ())

;;;###autoload
(defun +lsp/init-choose-client ()
  (interactive)
  (ivy-read "LSP Client: " '("lsp-mode" "lsp-bridge")
	    :preselect 0
	    :require-match t
	    :history '+lsp/choose-client-history
	    :caller '+lsp/choose-client
	    :action (lambda (client)
		      (interactive)
		      (pcase client
			("lsp-mode" ((lsp-deferred)))
			("lsp-bridge" ((lsp-bridge-mode +1)))))))




(provide 'implicit-lsp)
