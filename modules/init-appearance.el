;;; -*- lexical-binding: t -*-


;;;###autoload
(defun +eye-candy/insert-doom-color ()
  "Insert the rgb value of a doom theme color."
  (interactive)
  (consult--read doom-themes--colors
                 :prompt "Doom theme color: "
                 :lookup (lambda (color-entry &rest props)
                           (insert (-second-item color-entry)))))

(use-package doom-themes
  :demand
  :init
  (defun +colors/setup-doom-themes ()
    (progn
      (require 'doom-themes)
      (require 'f)
      (add-to-list 'custom-theme-load-path (f-join straight-base-dir
                                                   "straight"
                                                   straight-build-dir
                                                   "doom-gruber-darker-theme/"))
      (setq doom-themes-enable-bold t
            doom-themes-enable-italic t
            doom-themes-treemacs-enable-variable-pitch nil
            doom-themes-treemacs-theme "doom-atom")
      (doom-themes-visual-bell-config)
      (doom-themes-neotree-config)
      (doom-themes-treemacs-config)
      (doom-themes-org-config)
      (+colors/load-theme +base/theme)
      ))
  :hook
  (after-init . +colors/setup-doom-themes)
  (tty-setup . (lambda ()
                 (interactive)
                 (+colors/load-theme +base/theme))))

(use-package ef-themes)

(use-package solaire-mode
  :demand
  :custom-face
  (solaire-default-face ((t (:background ,(doom-darken 'solaire-default-face 0.1)))))
  :config
  (defun +colors/load-theme (theme)
    ""
    (interactive)
    (require 'solaire-mode)
    (load-theme theme t)
    (solaire-global-mode +1)))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package nerd-icons
  :general
  (+leader-keys
    "i i" '("Insert unicode icon" . nerd-icons-insert)))

(use-package rainbow-delimiters
  :hook ((prog-mode emacs-lisp-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook
  (css-mode . rainbow-mode)
  (css-ts-mode . rainbow-mode)
  (helpful-mode . rainbow-mode))

(use-package hl-todo
  :commands
  global-hl-todo-mode
  :init
  (setq hl-todo-keyword-faces
        `(("HOLD" . "#d0bf8f")
          ("TODO" . ,(doom-color 'green))
          ("NEXT" . "#dca3a3")
          ("THEM" . "#dc8cc3")
          ("PROG" . ,(doom-color 'teal))
          ("OKAY" . ,(doom-color 'teal))
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
  :hook
  (after-init . global-hl-todo-mode))

(use-package olivetti)

(use-package doom-gruber-darker-theme
  :straight (doom-gruber-darker-theme :type git
                                      :host github
                                      :repo "implicit-image/doom-gruber-darker-theme"))

;; font ligatures
(use-package ligature
  :config
  (ligature-set-ligatures '(haskell-mode ocaml-ts-mode agda-mode lean-mode)
                          '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                            "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" ">>=" "<=>" "<==>" "<===>" "<====>" "<!---"
                            "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                            ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (ligature-set-ligatures '(js-ts-mode typescript-ts-mode web-mode html-ts-mode)
                          '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                            "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" ">>=" "<=>" "<==>" "<===>" "<====>" "<!---"
                            "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                            ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))


(use-package face-remap
  :straight nil
  :general
  (global-map
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease))

(use-package custom
  :straight nil
  :general
  (+leader-keys
    "h t" '("Load theme" . load-theme)))

(provide 'init-appearance)
