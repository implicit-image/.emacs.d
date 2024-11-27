(require 'anzu)
(require 'f)

(use-package yasnippet
  :config
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
  :init
  (setq doom-snippets-enable-short-helpers t))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package eldoc
  :init
  (setq eldoc-echo-area-prefer-doc-buffer t
	eldoc-echo-area-use-multiline-p nil))


(use-package eldoc-box
  :custom-face
  (eldoc-box-body ((t (:box nil))))
  ;; TODO: maybe remove some prettify functions from eldoc-box-buffer-hook, it looks wonky
  :init
  (setq eldoc-box-clear-with-C-g t)
  :config
  (set-face-attribute 'eldoc-box-body nil :inherit 'corfu-default)
  :hook
  (lsp-mode . eldoc-box-hover-at-point-mode)
  (emacs-lisp-mode . (lambda ()
		       (if (display-graphic-p)
			   (eldoc-box-hover-at-point-mode +1)
			 (eldoc-mode +1)))))

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package compile
  :straight nil
  :config
  (setq compilation-scroll-output t))

(use-package comint
  :straight nil
  :init
  (setq comint-eol-on-send t)
  :general
  (+leader-keys
    "!" '("Comint run" . comint-run)))

(provide 'init-coding)
