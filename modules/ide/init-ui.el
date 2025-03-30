;;; -*- lexical-binding: t -*-

(use-package context-menu-mode
  :straight nil
  :config

  (defun my-ctx-menu-addons (menu click)
    (when lsp-mode
      (define-key-after menu [lsp-hover]
        '(menu-item "LSP hover" lsp-ui-doc-show
                    :help "Show LSP Hover info")))
    menu)

  (add-hook 'context-menu-functions 'my-ctx-menu-addons)
  :hook
  (dired-mode . context-menu-mode)
  (prog-mode . context-menu-mode))

(use-package flycheck-overlay
  :disabled
  :straight (flycheck-overlay :type git
                              :host github
                              :repo "konrad1977/flycheck-overlay")
  :custom-face
  (flycheck-overlay-marker ((t (:foreground ,(doom-darken (doom-color 'fg) 0.1) :underline nil))))
  :init
  (setq flycheck-overlay-use-theme-colors t
        flycheck-overlay-virtual-line-type 'line-no-arrow
        flycheck-overlay-show-at-eol nil
        flycheck-overlay-hide-checker-name t
        flycheck-overlay-show-virtual-line t
        flycheck-overlay-virtual-line-icon nil
        flycheck-overlay-background-lightness 60
        flycheck-overlay-text-tint 'lighter
        flycheck-overlay-text-tint-percent 60
        flycheck-overlay-percent-darker 70
        flycheck-overlay-info-icon "ℹ"
        flycheck-overlay-debounce-interval 0.1
        flycheck-overlay-hide-when-cursor-is-on-same-line t
        flycheck-overlay-icon-right-padding 0.9)
  :hook
  (flycheck-mode . flycheck-overlay-mode)
  :general
  (flycheck-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :non-normal-prefix "C-c SPC"
   "t o" '("Toggle flycheck overlay" . flycheck-overlay-toggle)))


(use-package sideline-flycheck
  :custom-face
  (sideline-flycheck-info ((t (:background ,(doom-color 'base2)))))
  (sideline-flycheck-warning ((t (:background ,(doom-color 'base2)))))
  (sideline-flycheck-error ((t (:background ,(doom-color 'base2)))))
  :init
  (setq sideline-flycheck-max-lines 1
        sideline-flycheck-show-checker-name nil))

(use-package sideline-blame
  :custom-face
  (sideline-blame ((t (:box (:line-width -1) :background ,(doom-color 'base3) :slant normal :size ,+base/font-size))))
  :init
  (setq sideline-blame-commit-format "* %s"))

(use-package sideline-lsp
  :custom-face
  (sideline-lsp-code-action ((t (:foreground ,(doom-color 'base5)))))
  :init
  (setq sideline-lsp-ignore-duplicate t
        sideline-lsp-code-actions-prefix "[!]"))

(use-package sideline
  :defer 10
  :init
  (setq sideline-backends-right '(sideline-lsp
                                  sideline-flycheck)
        sideline-backends-right-skip-current-line t
        sideline-order-right 'up
        sideline-format-right "    %s               "
        sideline-display-backend-name nil
        sideline-display-backend-format ""
        sideline-display-backend-type nil)
  :config
  (require 'sideline-lsp)
  (require 'sideline-flycheck)
  (require 'sideline-blame)
  (global-sideline-mode 1)
  :hook
  ((flycheck-mode lsp-mode prog-mode blamer-mode) . sideline-mode))

(use-package which-func
  :custom-face
  (header-line ((t (:background ,(doom-color 'bg-alt) :inherit nil))))
  :straight nil
  :init
  (setq which-func-display 'header
        which-func-update-delay 0.2
        which-func-format '("     "
                            (:propertize which-func-current
                                         face which-func mouse-face mode-line-highlight help-echo
                                         "Current function\nmouse-1: go to beginning\nmouse-2: toggle rest visibility\nmouse-3: go to end")
                            ""))
  (defun +which-func--setup ()
    (which-function-mode))
  :hook
  ((python-ts-mode c-ts-mode rust-ts-mode emacs-lisp-mode c++-ts-mode ) . +which-func--setup))

(provide 'init-ui)
;;; init-ui.el ends here
