;;; -*- lexical-binding: t -*-

;;;; packages for setting and exploring keybindings.

(use-package hydra)

(use-package which-key
  :commands
  (which-key-setup-side-window-bottom)
  :init
  (setq which-key-popup-type 'minibuffer
        which-key-idle-delay 0.1
        which-key-max-display-columns 5
        which-key-allow-evil-operators t
        which-key-add-column-padding 5
        which-key-show-remaining-keys t
        which-key-min-display-lines 6
        which-key-max-display-lines 6)
  ;; display `which-key' window on bottom side of the frame
  (which-key-setup-side-window-bottom)
  :config
  (which-key-mode)
  :hook
  ;; load after loading user init
  (after-init . which-key-mode))

(use-package general
  :demand
  :config
  (general-evil-setup)

  ;;;; key definer for global leader keys
  (general-create-definer +leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :global-prefix "C-c SPC")

  ;;;; key definer for local leader keys
  (general-create-definer +local-leader-keys
    :states '(normal insert visual emacs)
    :prefix "SPC m"
    :non-normal-prefix "M-SPC m"
    :global-prefix "C-c m")

  (general-create-definer +mode-keys
    :states '(normal insert visual emacs)
    :prefix ""
    :global-prefix "M-SPC"
    :non-normal-prefix "C-c")

  ;; (general-create-definer +mode-keys-normal)

  ;; (general-create-definer +mode-keys-visual)

  ;; (general-create-definer +mode-keys-insert)

  ;; (general-create-definer +mode-keys-emacs)

  ;;;; key categories
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
