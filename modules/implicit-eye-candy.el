(require 'f)

(defvar +eye-candy/custom-faces ()
  "Alist of custom faces to reload when the theme changes")

;; (
;;  ()
;;  )

;;(defun +eye-candy/reload-custom-faces ()
;;  (message "TODO: reloading custom faces"))


;;(use-package emacs
;;  :config
;;  ;; reload all custom faces after theme reload 
;;  (advice-add 'load-theme :after '+eye-candy/reload-custom-faces))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook
  (css-mode . rainbow-mode)
  (helpful-mode . rainbow-mode))

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces 
	`(("HOLD" . "#d0bf8f")
	  ("TODO" . "#cc9393")
	  ("NEXT" . "#dca3a3")
	  ("THEM" . "#dc8cc3")
	  ("PROG" . "#7cb8bb")
	  ("OKAY" . "#7cb8bb")
	  ("DONT" . "#5f7f5f")
	  ("FAIL" . "#8c5353")
	  ("DONE" . "#afd8af")
	  ("NOTE" . "#d0bf8f")
	  ("MAYBE" . "#d0bf8f")
	  ("KLUDGE" . "#d0bf8f")
	  ("HACK" . "#d0bf8f")
	  ("TEMP" . "#d0bf8f")
	  ("FIXME" . "#cc9393")
	  ("XXXX*" . "#cc9393")))
  (global-hl-todo-mode 1))

(use-package olivetti)

(use-package doom-gruber-darker-theme
  :demand
  :straight (doom-gruber-darker-theme :type git
                                      :host github
                                      :repo "implicit-image/doom-gruber-darker-theme")
  :after doom-themes)

(use-package doom-themes
  :demand
  :config
  (add-to-list 'custom-theme-load-path (f-join straight-base-dir
                                               "straight"
                                               straight-build-dir
                                               "doom-gruber-darker-theme/"))
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (load-theme 'doom-gruber-darker t))




;; font ligatures
(use-package ligature
  :config
  (ligature-set-ligatures 'org-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
				      "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
				      "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
				      ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))


(provide 'implicit-eye-candy)
