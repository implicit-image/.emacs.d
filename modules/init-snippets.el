(use-package yasnippet-capf)

(use-package tempel
  :preface
  (defun +tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons 'tempel-expand completion-at-point-functions)))
  :hook
  ((prog-mode conf-mode) . tempel-abbrev-mode)
  (tempel-abbrev-mode-hook . +tempel-setup-capf))

(use-package tempel-collection)

(use-package yasnippet
  :init
  (setq doom-snippets-enable-short-helpers t)
  :config
  (setq yas-snippet-dirs (append yas-snippet-dirs
                                 `(,(file-name-concat user-emacs-directory "straight" straight-build-dir "doom-snippets")
                                   ,(file-name-concat user-emacs-directory "snippets")
                                   ,(file-name-concat user-emacs-directory "straight" straight-build-dir "yasnippet-snippets/snippets"))))
  :hook
  ((prog-mode-hook conf-mode-hook snippet-mode-hook text-mode-hook) . yas-minor-mode)
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
  :init
  (with-eval-after-load 'yasnippet
    (yasnippet-snippets-initialize)))

(use-package eldoc
  :init
  (setq eldoc-echo-area-prefer-doc-buffer nil
        eldoc-idle-delay 0.1
        eldoc-echo-area-use-multiline-p nil))


(provide 'init-snippets)
