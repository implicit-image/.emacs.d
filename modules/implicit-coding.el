(require 'anzu)
(require 'f)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
				 `(,(f-join user-emacs-directory "straight" straight-build-dir "doom-snippets")
				   ,(f-join user-emacs-directory "snippets")
				   ,(f-join user-emacs-directory "straight" straight-build-dir "yasnippet-snippets/snippets"))))
  :config
  (yas-global-mode))

(use-package doom-snippets
  :after corfu
  :straight (doom-snippets :type git
			   :host github
			   :repo "doomemacs/snippets"
			   :files ("*.el" "*"))
  :after yasnippet)

(use-package yasnippet-snippets
  :after yasnippet)

(use-package eldoc
  :init
  (setq eldoc-echo-area-prefer-doc-buffer nil))


(use-package eldoc-box
  :custom-face
  (eldoc-box-body ((t (:box nil))))
  ;; TODO: maybe remove some prettify functions from eldoc-box-buffer-hook, it looks wonky
  :config
  (set-face-attribute 'eldoc-box-body nil :inherit 'corfu-default))

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package compile
  :straight nil
  :config
  (setq compilation-scroll-output t))

(provide 'init-coding)
