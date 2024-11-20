
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
(require 'implicit-elisp-libs)
;; base config
(require 'implicit-base)
;; keybindings
(require 'implicit-evil)
(require 'implicit-keybindings)
(require 'implicit-modules)
;; ui
(require 'implicit-eye-candy)
(require 'implicit-windows)
(require 'implicit-modeline)
;; programming
(require 'implicit-checkers)
(require 'implicit-coding)
(require 'implicit-completion)
(require 'implicit-editing)
(require 'implicit-indent)
(require 'implicit-lookup)
(require 'implicit-eglot)
(require 'implicit-lsp)
(require 'implicit-lsp-mode)
;; (require 'implicit-lsp-bridge)
;; language support
(require 'implicit-haskell)
(require 'implicit-rust)
(require 'implicit-lisp)
(require 'implicit-python)
(require 'implicit-R)
(require 'implicit-gleam)
(require 'implicit-erlang)
(require 'implicit-elixir)
(require 'implicit-go)
(require 'implicit-ruby)
(require 'implicit-web)
(require 'implicit-dotnet)
(require 'implicit-jvm)
(require 'implicit-zig)
(require 'implicit-assembly)
(require 'implicit-md)
(require 'implicit-c)
(require 'implicit-idris)
(require 'implicit-ada)
(require 'implicit-elm)
(require 'implicit-crystal)
(require 'implicit-dart)
;(require 'implicit-hdl)
(require 'implicit-ml)
(require 'implicit-latex)
(require 'implicit-shell)
(require 'implicit-conf)
(require 'implicit-misc-langs)
;; management
(require 'implicit-remote)
(require 'implicit-treemacs)
(require 'implicit-vc)
(require 'implicit-projects)
(require 'implicit-buffers)
(require 'implicit-dired)
(require 'implicit-workspaces)
;; org mode
(require 'implicit-org)
(require 'implicit-org-agenda)
(require 'implicit-org-roam)
(require 'implicit-org-babel)
;; utils
(require 'implicit-databases)
(require 'implicit-terminal)
(require 'implicit-tty)
(require 'implicit-search)
(require 'implicit-utils)
(require 'implicit-books)
(require 'implicit-llm)
(require 'implicit-treesitter)
;; apps
(require 'implicit-socials)
;; website export config
(require 'implicit-image-dot-github-dot-io)

;; load theme
