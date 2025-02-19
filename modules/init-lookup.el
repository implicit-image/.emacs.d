;;; -*- lexical-binding: t -*-

(require 'init-lookup-external-sources)

;;;; Alists of lookup functions
;;;; each list conatins (MAJOR-MODE . LOOKUP-FUNCTION) pairs
;;;; if current mode is not found, the default lookup mechanism, `+lookup/' is used

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

(defun +lookup-set-fn (type &rest forms)
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
(defun +lookup/popup ()
  "Display temporary documentation popup using a mode-dependent function."
  (interactive)
  (let ((lookup-function (alist-get major-mode +lookup/popup-functions-alist)))
    (cond (lookup-function (funcall-interactively lookup-function))
          ((memq 'lsp-ui-mode local-minor-modes) (lsp-ui-doc-glance))
          ((memq 'lsp-bridge-mode local-minor-modes) (lsp-bridge-popup-documentation))
          (t (message "No documentation function found")))))

;;;###autoload
(defun +lookup/in-buffer ()
  "Display temporary documentation buffer using a mode dependent function."
  (interactive)
  (let ((lookup-function (alist-get major-mode +lookup/buffer-functions-alist)))
    (cond (lookup-function (funcall-interactively lookup-function))
          ((memq 'lsp-mode local-minor-modes) (lsp-describe-thing-at-point))
          ((memq 'lsp-bridge-mode local-minor-modes) (lsp-bridge-popup-documentation-buffer))
          (t (message "No documentation function found")))))


;;;###autoload
(defun +lookup/documentation ()
  "Lookup documentation for symbol at point."
  (interactive)
  (let ((buffer-lookup-function (alist-get major-mode +lookup/buffer-functions-alist))
        (popup-lookup-function (alist-get major-mode +lookup/popup-functions-alist)))
    (cond ((and popup-lookup-function
                (display-graphic-p))
           (funcall-interactively popup-lookup-function))
          (buffer-lookup-function
           (funcall-interactively buffer-lookup-function))
          ((memq 'lsp-ui-mode local-minor-modes)
           (if (display-graphic-p)
               (lsp-ui-doc-show)
             (lsp-describe-thing-at-point)))
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

(defun +lookup/org-get-function ()
  "Get lookup function for language in current source block."
  (interactive)
  (if org-mode
      (let ((block-major-mode
             (intern (concat
                      (-first (org-babel-get-src-block-info))
                      "-mode"))))
        (+lookup--local-documentation block-major-mode))
    (message "Not in org mode")))

(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;; TODO: add :dash to use-package language mode declarations
(use-package dash-docs)

(use-package devdocs)

(use-package xref
  :general
  (+leader-keys
    "c g f" '("Xref forward" . xref-go-forward)
    "c g b" '("Xref back" . xref-go-back)))

(use-package info
  :straight nil
  :init
  (evil-set-initial-state 'Info-mode 'normal))

(use-package help-mode
  :straight nil
  :init
  (+windows-cfg '((help-mode)
                  :height 0.3
                  :position bottom
                  :dedicated nil
                  :stick nil
                  :noselect nil))
  :general
  (help-mode-map
   :states '(normal)
   "q" 'quit-window))

(use-package helpful
  :init
  (+lookup-set-fn 'buffer '(helpful-mode . helpful-at-point))
  ;;popwin support
  (+windows-cfg
   '((helpful-mode)
     :height 0.3 :position bottom :dedicated t :stick nil :noselect nil))
  :general
  (+leader-keys
    "h :" '("Describe command" . helpful-command)
    "h v" '("Describe variable" . helpful-variable)
    "h f" '("Describe function" . helpful-callable)
    "h k" '("Describe key" . helpful-key)
    "h s" '("Describe symbol" . helpful-symbol))
  (helpful-mode-map
   :states 'normal
   "q" 'quit-window))

(use-package dictionary
  :straight nil
  :init
  (+windows-cfg '(("\*Dictionary\*")
                  :position bottom :height 0.3))
  (setq dictionary-server "dict.org")
  :general
  (+leader-keys
    "h d" '("Dictionary" . dictionary-search)))

(use-package help-fns
  :straight nil
  :general
  (+leader-keys
    "h b" '("Describe bindings" . describe-bindings)
    "h F" '("Describe face" . describe-face)
    "h m" '("Describe keymap"   . describe-keymap)
    "h M" '("Describe mode"     . describe-mode)
    "h p" '("Describe package"  . describe-package)
    "h c" '("Describe character" . describe-char)))

(use-package eldoc
  :init
  (setq eldoc-echo-area-prefer-doc-buffer nil
        eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil))

(use-package eldoc-box
  :init
  (setq eldoc-box-clear-with-C-g t)
  :config
  (set-face-attribute 'eldoc-box-body nil :inherit 'corfu-default)
  :hook
  ((lsp-mode merlin-mode) . eldoc-box-hover-at-point-mode)
  (emacs-lisp-mode . (lambda ()
                       (if (display-graphic-p)
                           (eldoc-box-hover-at-point-mode +1)
                         (eldoc-mode +1)))))

(general-defs
  global-map
  :states '(visual normal)
  "K" '+lookup/documentation
  help-mode-map
  :states '(normal)
  "q" 'quit-window)

(provide 'init-lookup)
