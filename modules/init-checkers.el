;;; -*- lexical-binding: t -*-

(use-package flycheck
  :custom
  (flycheck-indication-mode nil)
  (flycheck-highlighting-mode 'symbols)
  (flycheck-idle-change-delay 0.5)
  (flycheck-display-errors-delay 1)
  :hook
  ((lsp-mode-hook emacs-lisp-mode-hook merlin-mode-hook) . flycheck-mode)
  (flycheck-error-list-mode-hook . visual-line-mode)
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

(use-package flyspell
  :straight nil
  :custom
  (flyspell-issue-welcoe-flag nil)
  (flyspell-issue-message-flag nil)
  :hook
  ((prog-mode-hook emacs-lisp-mode-hook) . flyspell-prog-mode)
  ((org-mode-hook markdown-mode-hook text-mode-hook) . flyspell-mode))

(provide 'init-checkers)
