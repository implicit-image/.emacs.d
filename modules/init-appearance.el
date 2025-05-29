;;; -*- lexical-binding: t -*-

(setq custom-theme-directory (expand-file-name (file-name-concat user-emacs-directory "straight" "build" "doom-gruber-darker")))

(defun +colors/setup-doom-themes ()
  (interactive)
  (progn
    (add-to-list 'custom-theme-load-path (file-name-concat straight-base-dir
                                                           "straight"
                                                           straight-build-dir
                                                           "doom-gruber-darker-theme/"))
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t
          doom-themes-treemacs-enable-variable-pitch nil
          doom-themes-treemacs-theme "doom-colors")
    (doom-themes-visual-bell-config)
    (doom-themes-treemacs-config)
    (doom-themes-org-config)
    (load-theme +base/theme t)))

(defun +custom-faces (theme)
  (custom-set-faces
   `(show-paren-match-expression ((t (:box nil :background ,(doom-color 'base4) :underline nil :overline nil))))
   `(secondary-selection ((t (:background ,(doom-color 'selection)))))
   `(acm-terminal-default-face ((t (:background "#343434"))))
   `(variable-pitch ((t (:foreground ,(doom-color 'fg-alt)))))
   `(whitespace-space ((t (:foreground ,(doom-color 'base4)))))
   `(whitespace-hspace ((t (:foreground ,(doom-color 'bg) :background ,(doom-color 'bg)))))
   `(whitespace-indentation ((t (:foreground ,(doom-color 'base4)))))
   `(company-tooltip ((t (:background ,(doom-darken (doom-color 'base4) 0.3)))))
   `(company-tooltip-selection ((t (:background ,(doom-color 'bg)))))
   `(corfu-border ((t (:background ,(doom-color 'fg-alt)))))
   `(corfu-echo ((t (:background ,(doom-color 'bg) :foreground ,(doom-color 'fg)))))
   `(lsp-lens-face ((t (:size ,+base/font-size))))
   `(lsp-signature-highlight-function-argument ((t (:underline t))))
   `(lsp-ui-peek-footer ((t (:background ,(doom-color 'bg)))))
   `(lsp-ui-peek-header ((t (:background ,(doom-color 'bg)))))
   `(lsp-ui-doc-background ((t :background ,(doom-color 'base0))))
   `(menu ((t (:background ,(doom-color 'bg-alt)) :foreground ,(doom-color 'fg))))
   `(header-line ((t (:background ,(doom-color 'bg-alt) :inherit nil))))
   `(sideline-lsp-code-action ((t (:foreground ,(doom-color 'base6)))))
   `(sideline-blame ((t (:box (:line-width -1) :background ,(doom-color 'base3) :slant normal :size ,+base/font-size))))
   `(sideline-flycheck-info ((t (:background ,(doom-color 'base2)))))
   `(sideline-flycheck-warning ((t (:background ,(doom-color 'base2)))))
   `(sideline-flycheck-error ((t (:background ,(doom-color 'base2)))))
   `(eldoc-box-border ((t (:background "black"))))
   `(devdocs-code-block ((t (:background ,(doom-color 'base4) :extend t))))
   `(treemacs-root-face ((t (:background ,(doom-color 'bg-alt) :foreground ,(doom-color 'blue) :extend t))))))

(add-hook 'enable-theme-functions '+custom-faces)


(use-package doom-themes
  :demand
  :hook
  (after-init-hook . +colors/setup-doom-themes)
  (tty-setup-hook . (lambda ()
                      (interactive)
                      (load-theme +base/theme t))))

;; (use-package all-the-icons)

;; (use-package solaire-mode
;;   :hook
;;   (after-init-hook . solaire-global-mode))

;; (use-package nerd-icons)

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

(use-package hl-todo
  :commands
  global-hl-todo-mode
  :init
  (with-eval-after-load 'doom-themes
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
  :hook
  (image-mode-hook . olivetti-mode))

(use-package doom-gruber-darker-theme
  :straight (doom-gruber-darker-theme :type git
                                      :host github
                                      :repo "implicit-image/doom-gruber-darker-theme"))

;; (use-package faces
;;   :straight nil
;; :custom-face
;; (variable-pitch ((t (:foreground ,(doom-color 'fg-alt))))))

(provide 'init-appearance)
