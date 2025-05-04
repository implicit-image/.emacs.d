;;; -*- lexical-binding: t -*-

(require 'init-lookup-external-sources)

;;;; Alists of lookup functions
;;;; each list contains (MAJOR-MODE . LOOKUP-FUNCTION) pairs
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
          (lspce-mode (lspce-help-at-point))
          (t (message "No documentation function found")))))

;;;###autoload
(defun +lookup/in-buffer ()
  "Display temporary documentation buffer using a mode dependent function."
  (interactive)
  (let ((lookup-function (alist-get major-mode +lookup/buffer-functions-alist)))
    (cond (lookup-function (funcall-interactively lookup-function))
          ((memq 'lsp-mode local-minor-modes) (lsp-describe-thing-at-point))
          ((memq 'lsp-bridge-mode local-minor-modes) (lsp-bridge-popup-documentation-buffer))
          (lspce-mode (lspce-help-at-point))
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
          ((or (bound-and-true-p eldoc-box-hover-at-point-mode)
               (bound-and-true-p eldoc-box-hover-mode))
           (eldoc-box-help-at-point))
          ((memq 'lsp-ui-mode local-minor-modes)
           (if (display-graphic-p)
               (lsp-ui-doc-glance)
             (lsp-describe-thing-at-point)))
          ((memq 'lsp-bridge-mode local-minor-modes)
           (if (display-graphic-p)
               (lsp-bridge-popup-documentation)
             (lsp-bridge-show-documentation)))
          ((bound-and-true-p lspce-mode) (lspce-help-at-point))
          (t (message "No documentation function found")))))

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

(use-package apropos
  :straight nil)

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
  :straight nil)

(use-package help-mode
  :straight nil
  :general
  (help-mode-map
   :states '(normal)
   "q" 'quit-window))

(use-package helpful
  :init
  (+lookup-set-fn 'buffer '(helpful-mode . helpful-at-point))
  ;; (setq helpful-switch-buffer-function )
  :general
  (+leader-keys
    "h :" '("Describe command" . helpful-command)
    "h v" '("Describe variable" . helpful-variable)
    "h f" '("Describe function" . helpful-callable)
    "h k" '("Describe key" . helpful-key)
    "h s" '("Describe symbol" . helpful-symbol))
  (helpful-mode-map
   :states 'normal
   "q" 'quit-window
   "<esc>" 'quit-window
   "ESC" 'quit-window))

(use-package dictionary
  :straight nil
  :init
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

  (setq eldoc-echo-area-prefer-doc-buffer 'maybe
        eldoc-idle-delay 0.05
        eldoc-echo-area-use-multiline-p 0.2)

  ;; (defun +eldoc--popup-function (docs interactive)
  ;;   (require 'popup)
  ;;   (let* (popup (popup-create (point) 10 10 ))))

  ;; (add-to-list 'eldoc-display-functions '+eldoc--popon-function)

  :hook
  (prog-mode-hook . eldoc-mode))

(use-package eldoc-box
  :disabled
  :custom-face
  (eldoc-box-border ((t (:background "black"))))
  :init
  (defvar +eldoc-minibuffer-display-modes '())

  (defun +eldoc--setup ()
    (interactive)
    (when (not (memq major-mode +eldoc-minibuffer-display-modes))
      (eldoc-box-hover-mode)))

  (defun +eldoc-box--position-function (width height)
    (let* ((pos (eldoc-box--default-at-point-position-function-1 width height))
           (x (car pos))
           (y (cdr pos))
           (line-h (line-pixel-height))
           (num-lines (/ height line-h)))
      (cond ((> num-lines 2) (cons (+ x 100) y))
            (t (cons (+ x 40) y)))))

  (setq eldoc-box-clear-with-C-g t
        eldoc-box-only-multi-line nil
        eldoc-box-max-pixel-height (* (line-pixel-height) 10)
        eldoc-box-position-function '+eldoc-box--position-function)

  :config
  (set-face-attribute 'eldoc-box-body nil :inherit 'corfu-default)
  :hook
  (eldoc-mode-hook . +eldoc--setup))

(general-defs
  global-map
  :states '(visual normal)
  "K" '+lookup/documentation
  "C-k" '+lookup/in-buffer
  help-mode-map
  :states '(normal)
  "q" 'quit-window)

(provide 'init-lookup)
