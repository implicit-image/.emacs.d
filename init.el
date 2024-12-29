
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


(require 'init-modules (format "%smodules/init-modules.el" user-emacs-directory))
(require 'init-elisp-libs)
(require 'init-keybindings)
(require 'init-base)
(require 'init-evil)
(require 'init-treesitter)

;;;; appearance
(require 'init-appearance)
(require 'init-windows)
(require 'init-modeline)

;;; minibuffer completion
(require 'init-completion)

;;;; programming utilities
(require 'init-lookup)
(require 'init-ide)
(require 'init-languages)
(require 'init-projects)

;;;; version control
(require 'init-vc)

;;;; terminal
(require 'init-terminal)

;;;; browsing
(require 'init-files)
(require 'init-buffers)
(require 'init-treemacs)
(require 'init-dired)
(require 'init-remote)

;;;; org mode
(require 'init-org)

;;;; tty emacs options
(require 'init-tty)

;;;; books
(require 'init-books)

;;;; llm integration
(require 'init-llm)

;;; social media clients
(require 'init-socials)
(require 'init-media)
