;;; -*- lexical-binding: t -*-

(defvar +lsp/installers '((npm-global . "npm install -g %s"))
  "Alist of (SYMBOL . COMMAND-STRING) where COMMAND-STRING is a command to install a server.")

(defvar +lsp-type-project-plist ())

;;;###autoload
(defun +lsp/init-choose-client ()
  (interactive)
  nil)
  ;; (let ((root (projectile-project-root))
  ;;       (lsp-type (plist-get '+lsp-type-project-plist  ))))
  ;; (consult--read '("lsp-mode" "lsp-bridge" "eglot" "none")
  ;;                :prompt "LSP client: "
  ;;                :require-match t
  ;;                :history '+lsp/choose-client-history
  ;;                :lookup (lambda (client)
  ;;                          (interactive)
  ;;                          (pcase client
  ;;                            ("lsp-mode" ((lsp-deferred)))
  ;;                            ("eglot" ((eglot)))
  ;;                            ("lsp-bridge" ((lsp-bridge-mode +1)))
  ;;                            ("lspce" ((lspce-mode +1)))
  ;;                            ("none" ((message "No LSP client chosen.")))))))


(add-hook 'find-file-hook '+lsp/init-choose-client)




(provide 'init-lsp)
