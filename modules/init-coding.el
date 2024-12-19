;;; -*-lexical-binding:t-*-

(use-package yasnippet
  :config
  (require 'f)
  (setq yas-snippet-dirs (append yas-snippet-dirs
				 `(,(f-join user-emacs-directory "straight" straight-build-dir "doom-snippets")
				   ,(f-join user-emacs-directory "snippets")
				   ,(f-join user-emacs-directory "straight" straight-build-dir "yasnippet-snippets/snippets"))))
  :config
  (yas-global-mode)
  :general
  (+leader-keys
    "i s" '("Insert snippet" . yas-insert-snippet)))



(use-package doom-snippets
  :after corfu
  :straight (doom-snippets :type git
			   :host github
			   :repo "doomemacs/snippets"
			   :files ("*.el" "*"))
  :config
  (setq doom-snippets-enable-short-helpers t))

(use-package yasnippet-snippets
  :after yasnippet)

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

(use-package dap-mode
  :config (dap-auto-configure-mode))

(use-package dape
  :init
  (setq dape-key-prefix "C-x D")
  :config
  (setq dape-buffer-window-arrangement 'right))

(use-package compile
  :straight nil
  :init
  (+windows-cfg '((compilation-mode)
		  :position bottom :height 0.3 :noselect nil :dedicated t))
  :config
  (setq compilation-scroll-output t))

(use-package comint
  :straight nil
  :init
  (setq comint-eol-on-send t)
  :general
  (+leader-keys
    "!" '("Comint run" . comint-run)))



(+leader-keys
  "c g f" '("Xref forward" . xref-go-forward)
  "c g b" '("Xref back" . xref-go-back))

(provide 'init-coding)
