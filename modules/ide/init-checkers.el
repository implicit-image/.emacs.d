;;; -*- lexical-binding: t -*-

(use-package flycheck
  :custom
  (flycheck-indication-mode nil)
  (flycheck-highlighting-mode 'symbols)
  (flycheck-idle-change-delay 0.5)
  :init
  (+windows-cfg '((flycheck-mode-major-mode)
                  :position bottom :height 0.3))
  (+windows-cfg '((flycheck-error-list-mode)
                  :position bottom :height 0.4 :noselect nil))
  :hook
  ((lsp-mode emacs-lisp-mode merlin-mode) . flycheck-mode)
  (flycheck-error-list-mode . visual-line-mode)
  :general
  (+mode-keys
    :keymaps 'flycheck-mode-map
    "]" 'flycheck-next-error
    "[" 'flycheck-previous-error))


(use-package consult-flycheck
  :straight (consult-flycheck :type git
                              :host github
                              :repo "minad/consult-flycheck")
  :general
  (flycheck-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "s e" '("Consult errors" . consult-flycheck)))

(use-package flymake)

(use-package flymake-collection)

(use-package flyspell
  :straight nil
  :hook
  ((prog-mode emacs-lisp-mode) . flyspell-prog-mode)
  ((org-mode markdown-mode text-mode) . flyspell-mode))

(provide 'init-checkers)
