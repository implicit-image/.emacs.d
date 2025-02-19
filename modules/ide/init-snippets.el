
(use-package yasnippet-capf
  :init
  (add-hook 'completion-at-point-functions #'yasnippet-capf)
  (defun +snippets/yasnippet-capf-setup ()
    (require 'yasnippet-capf))
  :hook
  (corfu-mode . +snippets/yasnippet-capf-setup))

(use-package tempel
  :init
  (defun +snippets/tempel-setup ()
    (require 'tempel)
    (require 'tempel-collection))
  :hook
  (corfu-mode . +snippets/tempel-setup))

(use-package tempel-collection)

(use-package yasnippet
  :init
  (setq doom-snippets-enable-short-helpers t)
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


(provide 'init-snippets)
