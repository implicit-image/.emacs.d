;;; -*- lexical-binding: t -*-

(defvar +lookup/buffer-functions-alist nil
  "alist of form (MAJOR-MODE . FUNCTION).")

(defvar +lookup/popup-functions-alist nil
  "alist of form (MAJOR-MODE . FUNCTION).")

(defvar +lookup/find-def-functions-alist nil
  "alist of form (MAJOR-MODE . FUNCTION).")

(defvar +lookup/find-ref-functions-alist nil
  "alist of form (MAJOR-MODE . FUNCTION)")

(defvar +lookup/find-impl-functions-alist nil
  "alist of form (MAJOR-MODE . FUNCTION).")

(setopt max-mini-window-height 0.1
        browse-url-firefox-program (+os/per-system! :wsl "/mnt/c/Program Files/Mozilla Firefox/firefox.exe"
                                                    :win "firefox.exe"
                                                    :linux "firefox")
        dictionary-server "dict.org")

(use-package webjump
  :custom
  (webjump-sites '(("DuckDuckGo" . [simple-query "www.duckduckgo.com" "www.duckduckgo.com/?q=" ""])
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
                   ("ChatGPT" . [simple-query "https://chatgpt.com" "https://chatgpt.com/?q=" ""]))))

(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer nil)
  (eldoc-idle-delay 0.4)
  (eldoc-documentation-strategy 'eldoc-documentation-compose)
  (eldoc-echo-area-use-multiline-p nil)
  :config
  (add-to-list 'eldoc-display-functions '+eldoc-display-in-popup))

(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :hook
  (xref-backend-functions . dumb-jump-xref-activate))

(use-package devdocs)

(use-package xref
  :custom
  (xref-search-program 'ripgrep)
  :bind*
  (("M-g r" . xref-find-references)
   ("M-g d" . xref-find-definitions)
   :map meow-jump-global-map
   ("r" . xref-find-references)
   ("d" . xref-find-definitions)))

(use-package helpful
  :config
  (+lookup-set-fn! buffer (helpful-mode . helpful-at-point))
  :bind
  (([remap describe-command] . helpful-command)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-key] . helpful-key)
   ([remap describe-symbol] . helpful-symbol))
  :bind*
  ( :map help-map
    ("f" . helpful-callable)))

(use-package help
  :straight nil
  :custom
  (help-enable-completion-autoloads nil)
  (help-enable-autoload nil)
  (help-enable-symbol-autoload nil)
  :bind*
  ( :map help-map
    ("b" . describe-bindings)
    ("F" . describe-face)
    ("m" . describe-keymap)
    ("c" . describe-char)
    ("t" . describe-text-properties)
    ("\\" . describe-input-method)
    ("p" . describe-package)
    ("T" . describe-theme)
    ("M" . describe-mode)
    ("i" . info-lookup-symbol)
    ("w" . woman)))

(provide 'init-lookup)
