(defvar +lookup/buffer-functions-alist '()
  "alist of form (MAJOR-MODE . FUNCTION).")

(defvar +lookup/popup-functions-alist '()
  "alist of form (MAJOR-MODE . FUNCTION).")

(defvar +lookup/find-def-functions-alist '()
  "alist of form (MAJOR-MODE . FUNCTION).")

(defvar +lookup/find-ref-functions-alist '()
  "alist of form (MAJOR-MODE . FUNCTION).")

(defvar +lookup/find-impl-functions-alist '()
  "alist of form (MAJOR-MODE . FUNCTION).")

;;;###autoload
(defun +lookup/popup ()
  "Display temporary documentation popup using a mode-dependent function."
  (interactive)
  (let ((lookup-function (alist-get major-mode +lookup/popup-functions-alist)))
    (cond (lookup-function (funcall-interactively lookup-function))
	  ((memq 'lsp-bridge-mode local-minor-modes) (lsp-bridge-popup-documentation))
	  (t (message "No documentation function found")))))

;;;###autoload
(defun +lookup/in-buffer ()
  "Display temporary documentation buffer using a mode dependent function."
  (interactive)
  (let ((lookup-function (alist-get major-mode +lookup/buffer-functions-alist)))
    (cond (lookup-function (funcall-interactively lookup-function))
	  ((memq 'lsp-bridge-mode local-minor-modes) (lsp-bridge-popup-documentation-buffer))
	  (t (message "No documentation function found")))))

(defun +lookup-set-fn (type forms)
  ""
  (let ((fn-list (pcase type
		   ('popup '+lookup/popup-functions-alist)
		   ('buffer '+lookup/buffer-functions-alist)
		   ('ref '+lookup/find-ref-functions-alist)
		   ('def '+lookup/find-def-functions-alist)
		   ('impl '+lookup/find-impl-functions-alist)
		   (_ nil))))
    (when fn-list
      (mapc (lambda (form)
	      (add-to-list fn-list form))
	    forms))))

;;;###autoload
(defun +lookup/documentation ()
  "Lookup documentation for symbol at point."
  (interactive)
  (let ((buffer-lookup-function (alist-get major-mode +lookup/buffer-functions-alist))
	(popup-lookup-function (alist-get major-mode +lookup/popup-functions-alist)))
    (cond (popup-lookup-function
	   (funcall-interactively popup-lookup-function))
	  (buffer-lookup-function
	   (funcall-interactively buffer-lookup-function))
	  ((memq 'lsp-ui-mode local-minor-modes)
	   (lsp-ui-doc-glance))
	  ((memq 'lsp-bridge-mode local-minor-modes)
	   (lsp-bridge-show-documentation))
	  (t (message "No documentation function found")))))

;;;###autoload
(defun +lookup/find-references ()
  "Lookup references of symbol at point.")

;;;###autoload
(defun +lookup/find-definition ()
  "Lookup definition of symbol at point.")

;;;###autoload
(defun +lookup/find-implementation ()
  "Lookup implementation of symbol at point.")

;;;###autoload
(defun +lookup/peek-references ()
  "Peek the references")

;;;###autoload
(defun +lookup/peek-definition ()
  "Peek definition of symbol at point.")

;;;###autoload
(defun +lookup/peek-implementation ()
  "Peek implementation of symbol at point.")

(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package ivy-xref
  :after ivy
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; TODO: add :dash to use-package language mode declarations
(use-package dash-docs
  :demand)

(use-package info
  :straight nil
  :init
  (evil-set-initial-state 'Info-mode 'normal)
  :hook (Info-mode . (lambda ()
		       (interactive)
		       (display-line-numbers-mode -1))))

(use-package helpful
  :init
  (+lookup-set-fn 'buffer '((helpful-mode . helpful-at-point)))
  ;;popwin support
  (+windows-cfg
   '(("\*helpful*")
     :regexp t :height 0.3 :position bottom :dedicated nil :stick nil :noselect nil))
  :hook
  (helpful-mode . (lambda () (display-line-numbers-mode -1))))

(use-package dictionary
  :straight nil
  :init
  (+windows-cfg '(("\*Dictionary\*")
		  :position bottom :height 0.3))
  (setq dictionary-server "dict.org")
  :hook
  (dictionary-mode . (lambda ()
		       (display-line-numbers-mode -1))))

(provide 'implicit-lookup)
