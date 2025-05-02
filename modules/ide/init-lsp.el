;;; -*- lexical-binding: t -*-

(defvar +lsp/installers '((npm-global . "npm install -g %s"))
  "Alist of (SYMBOL . COMMAND-STRING) where COMMAND-STRING is a command to install a server.")

(defvar +lsp-type-project-plist ())

(defvar +lsp-client-alist nil)


(defun +lsp/client-setup ()
  "Setup client for current buffer."
  (interactive)
  (let ((client (alist-get major-mode +lsp-client-alist)))
    (when client
      (funcall-interactively client))))


(provide 'init-lsp)
