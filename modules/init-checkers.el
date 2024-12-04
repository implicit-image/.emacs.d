;;; -*- lexical-binding: t -*-

(use-package flyspell)


(defun +checkers--get-pretty-errors ()
  "Get propertized errors from `flycheck'."
  (require 'flycheck)
  (mapcar
   (lambda (err)
     (let ((file-name (propertize (file-name-nondirectory (flycheck-error-filename err)) 'face 'flycheck-error-list-id))
	   (line-number (propertize (number-to-string (flycheck-error-line err)) 'face 'flycheck-error-list-info))
	   (error-message (propertize (flycheck-error-message err) 'face 'flycheck-error-list-checker-name)))
       (concat file-name
	       ":"
	       line-number
	       ":"
	       error-message)))
   flycheck-current-errors))

;;;###autoload
(defun +checkers/counsel-errors ()
  ""
  (interactive)
  (require 'ivy)
  (ivy-read "Errors: "
	    (+checkers--get-pretty-errors)
	    :preselect 1
	    :require-match t
	    :action (lambda (err)
		      (flycheck-goto-line
		       (string-to-number
			(string-split err ":"))))
	    :caller '+checkers/counsel-errors))



(use-package flycheck
  :init
  (+windows-cfg '((flycheck-mode-major-mode)
		  :position bottom :height 0.3 ))
  :hook
  ((lsp-mode emacs-lisp-mode) . flycheck-mode)
  :general
  (flycheck-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "s e" '("Flycheck errors" . +checkers/counsel-errors))
  (flycheck-mode-map
   :states '(normal visual insert)
   "C-c ]" 'flycheck-next-error
   "C-c [" 'flycheck-previous-error))

(use-package sideline-flycheck
  :after flycheck
  :init
  (setq sideline-backends-right '(sideline-flycheck)
	sideline-flycheck-max-lines 2)
  :hook
  (flycheck-mode . sideline-mode)
  (flycheck-mode . sideline-flycheck-setup))

(use-package flymake)

(provide 'init-checkers)
