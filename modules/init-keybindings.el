(use-package hydra)

(use-package which-key
  :demand
  :commands
  (which-key-setup-side-window-bottom)
  :init
  (setq which-key-popup-type 'minibuffer
	which-key-idle-delay 0.6
	which-key-max-display-columns 5
	which-key-add-column-padding 10
	which-key-min-display-lines 5)
  (which-key-setup-side-window-bottom)
  :config
  (which-key-mode))

(use-package general
  :demand
  :config
  (general-evil-setup)
  (general-create-definer +leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")
  (general-create-definer +local-leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC m"
    :global-prefix "M-SPC m")
  ;; setup key categories
  (+leader-keys
    "a" '(:ignore t :which-key "applications")
    "b" '(:ignore t :which-key "buffer")
    "c" '(:ignore t :which-key "code")
    "d" '(:ignore t :which-key "dired")
    "f" '(:ignore t :which-key "file")
    "g" '(:ignore t :which-key "git")
    "h" '(:ignore t :which-key "help")
    "i" '(:ignore t :which-key "insert")
    "n" '(:ignore t :which-key "notes")
    "o" '(:ignore t :which-key "open")
    "p" '(:ignore t :which-key "projects")
    "q" '(:ignore t :which-key "quit")
    "s" '(:ignore t :which-key "search")
    "t" '(:ignore t :which-key "toggle")
    "w" '(:ignore t :which-key "window")))


(provide 'init-keybindings)
