
;; set up straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(straight-use-package 'use-package)

;; make use-package use straight.el by default
(setq straight-use-package-by-default t
      ;; lazy load by default
      use-package-always-defer t)

(setq +modules/path (expand-file-name "./modules/" user-emacs-directory))

(add-to-list 'load-path +modules/path)


;; useful elisp libraries
(require 'init-elisp-libs)
;; base config
(require 'init-base)
;; keybindings
(require 'init-keybindings)
(require 'init-evil)
(require 'init-modules)
;; ui
(require 'init-eye-candy)
(require 'init-windows)
(require 'init-modeline)
;; programming
(require 'init-checkers)
(require 'init-coding)
(require 'init-completion)
(require 'init-editing)
(require 'init-indent)
(require 'init-lookup)
(require 'init-eglot)
(require 'init-lsp)
(require 'init-lsp-mode)
;; (require 'init-lsp-bridge)
;; management
(require 'init-remote)
(require 'init-treemacs)
(require 'init-vc)
(require 'init-projects)
(require 'init-buffers)
(require 'init-dired)
;; utils
(require 'init-databases)
(require 'init-terminal)
(require 'init-tty)
(require 'init-search)
(require 'init-utils)
(require 'init-books)
(require 'init-llm)
(require 'init-treesitter)
;; language support
(require 'init-haskell)
(require 'init-rust)
(require 'init-lisp)
(require 'init-python)
(require 'init-R)
(require 'init-gleam)
(require 'init-erlang)
(require 'init-elixir)
(require 'init-go)
(require 'init-ruby)
(require 'init-web)
(require 'init-dotnet)
(require 'init-jvm)
(require 'init-zig)
(require 'init-assembly)
(require 'init-md)
(require 'init-c)
(require 'init-idris)
(require 'init-ada)
(require 'init-elm)
(require 'init-crystal)
(require 'init-dart)
(require 'init-hdl)
(require 'init-ml)
(require 'init-latex)
(require 'init-shell)
(require 'init-conf)
(require 'init-misc-langs)
;; org mode
(require 'init-org)
(require 'init-org-agenda)
(require 'init-org-roam)
(require 'init-org-babel)
;; apps
(require 'init-socials)
(require 'init-emms)
;; website export config
(require 'init-implicit-image-dot-github-dot-io)

;; load theme
