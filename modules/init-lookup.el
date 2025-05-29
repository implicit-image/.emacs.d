;;; -*- lexical-binding: t -*-

(defmacro +lookup-set-fn! (type &rest forms)
  "Set lookup function of TYPE in FORMS."
  (let ((fn-list (pcase type
                   ('popup '+lookup/popup-functions-alist)
                   ('buffer '+lookup/buffer-functions-alist)
                   ('ref '+lookup/find-ref-functions-alist)
                   ('def '+lookup/find-def-functions-alist)
                   ('impl '+lookup/find-impl-functions-alist)
                   (_ nil))))
    (when fn-list
      `(mapc (lambda (form)
               (add-to-list ',fn-list form))
             ',forms))))

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

;; (use-package apropos
;;   :straight nil)

(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :hook
  (xref-backend-functions . dumb-jump-xref-activate))

;; TODO: add :dash to use-package language mode declarations
;; (use-package dash-docs)

(use-package devdocs)
;; :custom-face
;; (devdocs-code-block ((t (:background ,(doom-color 'base4) :extend t)))))

(use-package xref
  :custom
  (xref-search-program 'ripgrep))
;; :bind*
;; (("C-x SPC c g f" . xref-go-forward)
;;  ("C-x SPC c g b" . xref-go-back)))

;; (use-package info
;;   :straight nil)
;;
;; (use-package help-mode
;;   :straight nil)

(use-package helpful
  :init
  (+lookup-set-fn! buffer (helpful-mode . helpful-at-point)))
;; :bind*
;; ( :map help-map
;;   (":" . helpful-command)
;;   ("v" . helpful-variable)
;;   ("f" . helpful-callable)
;;   ("k" . helpful-key)
;;   ("M" . helpful-mode)
;;   ("s" . helpful-symbol)))

;; (use-package dictionary
;;   :straight nil
;;   :init
;;   (setq dictionary-server "dict.org"))
(setopt dictionary-server "dict.org")
;; :bind*
;; ( :map help-map
;;   ("d" . dictionary-search)))

;; (use-package help-fns
;;   :straight nil)
;; :bind*
;; ( :map help-map
;;   ("b" . describe-bindings)
;;   ("F" . describe-face)
;;   ("m" . describe-keymap)
;;   ("p" . describe-package)
;;   ("c" . describe-char)))

(use-package eldoc
  :init
  (setq eldoc-echo-area-prefer-doc-buffer 'maybe
        eldoc-idle-delay 0.05
        eldoc-echo-area-use-multiline-p 0.2)
  :hook
  (prog-mode-hook . eldoc-mode))

(use-package eldoc-box
  :disabled
  ;; :custom-face
  ;; (eldoc-box-border ((t (:background "black"))))
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

(use-package browse-url
  :init
  (setq browse-url-firefox-program (+os/per-system! :wsl "/mnt/c/Program Files/Mozilla Firefox/firefox.exe"
                                                    :win "/mnt/c/Program Files/Mozilla Firefox/firefox.exe"
                                                    :linux "firefox")))

(setopt webjump-sites
        '(("DuckDuckGo" . [simple-query "www.duckduckgo.com" "www.duckduckgo.com/?q=" ""])
          ("Google" . [simple-query "www.google.com" "www.google.com/search?q=" ""])
          ("YouTube" . [simple-query "www.youtube.com/feed/subscriptions" "www.youtube.com/rnesults?search_query=" ""])
          ("Google" . [simple-query "https://www.google.com/search" "https://www.google.com/search?hl=en&q=" ""])
          ("Stack Overflow" . [simple-query "https://duckduckgo.com/?q=site%3Astackoverflow.com+" "https://duckduckgo.com/?q=site%3Astackoverflow.com+" ""])
          ("MyNixos" . [simple-query "https://mynixos.com" "https://mynixos.com/search?q=" ""])
          ("Wikipedia" . [simple-query "https://en.wikipedia.org" "https://en.wikipedia.org/w/index.php?search=" ""])
          ("CSS Tricks" . [simple-query "https://css-tricks.com" "https://css-tricks.com/?s=" ""])
          ("Python docs" . [simple-query "https://docs.python.org/3" "https://docs.python.org/3/search.html?q=" ""])
          ("DevDocs.io" . [simple-query "https://devdocs.io" "https://devdocs.io/#q=" ""])
          ("Rust STD Docs" . [simple-query "https://doc.rust-lang.org/stable" "https://doc.rust-lang.org/stable/std/index.html?search=" ""])
          ("Hoogle" . [simple-query "https://hoogle.haskell.org" "https://hoogle.haskell.org/?hoogle=" ""])
          ("ChatGPT" . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""])))

;; (use-package webjump
;;   :straight nil
;;   :custom
;;   (webjump-sites
;;    '(("DuckDuckGo" . [simple-query "www.duckduckgo.com" "www.duckduckgo.com/?q=" ""])
;;      ("Google" . [simple-query "www.google.com" "www.google.com/search?q=" ""])
;;      ("YouTube" . [simple-query "www.youtube.com/feed/subscriptions" "www.youtube.com/rnesults?search_query=" ""])
;;      ("Google" . [simple-query "https://www.google.com/search" "https://www.google.com/search?hl=en&q=" ""])
;;      ("Stack Overflow" . [simple-query "https://duckduckgo.com/?q=site%3Astackoverflow.com+" "https://duckduckgo.com/?q=site%3Astackoverflow.com+" ""])
;;      ("MyNixos" . [simple-query "https://mynixos.com" "https://mynixos.com/search?q=" ""])
;;      ("Wikipedia" . [simple-query "https://en.wikipedia.org" "https://en.wikipedia.org/w/index.php?search=" ""])
;;      ("CSS Tricks" . [simple-query "https://css-tricks.com" "https://css-tricks.com/?s=" ""])
;;      ("Python docs" . [simple-query "https://docs.python.org/3" "https://docs.python.org/3/search.html?q=" ""])
;;      ("DevDocs.io" . [simple-query "https://devdocs.io" "https://devdocs.io/#q=" ""])
;;      ("Rust STD Docs" . [simple-query "https://doc.rust-lang.org/stable" "https://doc.rust-lang.org/stable/std/index.html?search=" ""])
;;      ("Hoogle" . [simple-query "https://hoogle.haskell.org" "https://hoogle.haskell.org/?hoogle=" ""])
;;      ("ChatGPT" . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""])))
;;   :init
;;
;;   (defun +webjump (&optional at-point)
;;     (interactive)
;;     (let* ((site (assoc-string
;;                   (completing-read "Site: " webjump-sites nil t)
;;                   webjump-sites t))
;;            (name (car site))
;;            (expr (cdr site))
;;            (fun (if webjump-use-internal-browser
;;                     (apply-partially #'browse-url-with-browser-kind 'internal)
;;                   #'browse-url))
;;            (thing (if (bound-and-true-p at-point)
;;                       (thing-at-point symbol t)
;;                     (webjump-url-fix
;;                      (cond ((not expr) "")
;;                            ((stringp expr) expr)
;;                            ((vectorp expr) (webjump-builtin expr name))
;;                            ((listp expr) (eval expr t))
;;                            ((symbolp expr)
;;                             (if (fboundp expr)
;;                                 (funcall expr name)
;;                               (error "WebJump URL function \"%s\" undefined"
;;                                      expr)))
;;                            (t (error "WebJump URL expression for \"%s\" invalid"
;;                                      name)))))))
;;       (funcall fun thing))))
;; ;; :bind*
;; (("C-x <space> s W" . webjump)
;;  ("M-RET" . webjump)))

;; (bind-keys*
;;  ("M-k" . +lookup/in-buffer)
;;  ("M-K" . +lookup/documentation))

(provide 'init-lookup)
