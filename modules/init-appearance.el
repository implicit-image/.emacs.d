;;; -*- lexical-binding: t -*-

(setq custom-theme-directory
      (expand-file-name (file-name-concat user-emacs-directory "straight" "build" "doom-gruber-darker")))

(add-hook 'prog-mode 'prettify-symbols-mode)

(use-package doom-themes
  :demand
  :hook
  (after-init-hook . +appearance-setup-doom-themes))

(use-package rainbow-delimiters
  :hook ((prog-mode-hook emacs-lisp-mode-hook) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook
  ((css-mode-hook
    css-ts-mode-hook
    help-mode-hook
    lsp-help-mode-hook
    helpful-mode-hook)
   . rainbow-mode))

(use-package ultra-scroll
  :straight (ultra-scroll :type git
                          :host github
                          :repo "jdtsmith/ultra-scroll")
  :init
  (setq scroll-conservatively 101
        scroll-margin 0)
  :hook
  (after-init-hook . ultra-scroll-mode))

(use-package face-remap
  :straight nil
  :init
  (setq text-scale-mode-step 1.1)
  :bind*
  (("C-=" . text-scale-increase)
   ("C--" . text-scale-decrease)))

(use-package hl-todo
  :commands
  global-hl-todo-mode
  :init
  (with-eval-after-load (intern (concat (symbol-name +base/theme) "-theme"))
    (setq hl-todo-keyword-faces
          `(("HOLD" . "#d0bf8f")
            ("TODO" . ,(doom-color 'green))
            ("NEXT" . "#dca3a3")
            ("THEM" . "#dc8cc3")
            ("PROG" . ,(doom-color 'dark-blue))
            ("OKAY" . ,(doom-color 'blue))
            ("DONT" . "#5f7f5f")
            ("FAIL" . compilation-error)
            ("DONE" . "#afd8af")
            ("NOTE" . "#d0bf8f")
            ("MAYBE" . "#d0bf8f")
            ("KLUDGE" . warning)
            ("HACK" . warning)
            ("TEMP" . warning)
            ("FIXME" . compilation-error)
            ("XXXX*" . compilation-error)))
    (global-hl-todo-mode 1)))

(use-package olivetti
  :custom
  (olivetti-style 'fancy)
  (olivetti-body-width 90)
  :bind*
  ( :map meow-toggle-global-map
    ("o" . olivetti-mode)))

(use-package doom-gruber-darker-theme
  :straight (doom-gruber-darker-theme :type git
                                      :host github
                                      :repo "implicit-image/doom-gruber-darker-theme"))

(provide 'init-appearance)
