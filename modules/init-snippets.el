;;; -*- lexical-binding: t -*-

(+when-idle! 3.0 (require 'yasnippet))
(+when-idle! 2.0 (require 'yasnippet-snippets))

(use-package yasnippet-capf)

(use-package yasnippet
  :init
  (setq doom-snippets-enable-short-helpers t
        yas-snippet-dirs (list (file-name-concat user-emacs-directory "straight" straight-build-dir "doom-snippets")
                               (file-name-concat user-emacs-directory "snippets")
                               (file-name-concat user-emacs-directory "straight" straight-build-dir "yasnippet-snippets/snippets")))
  :bind*
  ( :map meow-insert-global-map
    ("s" . yas-insert-snippet))
  :hook
  ((prog-mode-hook conf-mode-hook snippet-mode-hook text-mode-hook) . yas-minor-mode))

(use-package doom-snippets
  :straight (doom-snippets :type git
                           :host github
                           :repo "doomemacs/snippets"
                           :files ("*.el" "*"))
  :init
  (setq doom-snippets-enable-short-helpers t))

(use-package yasnippet-snippets
  :init
  (with-eval-after-load 'yasnippet
    (yasnippet-snippets-initialize)))

(provide 'init-snippets)
