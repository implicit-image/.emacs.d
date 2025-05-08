;;; -*- lexical-binding: t -*-

(defun +decide-ibuffer-filter-groups ()
  (interactive)
  (let ((group "default"))
    (ibuffer-switch-to-saved-filter-groups group)))


;; add default config filenames to correct automodes

(defvar +buffers/special-auto-mode-alist ()
  "Alist of special file names and corresponding major modes.")



(use-package ibuffer
  :straight nil
  :init
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-saved-filter-groups '(("default"
                                       ("org" (or
                                               (mode . org-mode)
                                               (name . "^\\*Org Src")
                                               (name . "^\\*Org Agenda\\*$")))
                                       ("tramp" (name . "^\\*tramp.*"))
                                       ("emacs" (or
                                                 (name . "^\\*scratch\\*$")
                                                 (name . "^\\*Messages\\*$")
                                                 (name . "^\\*Warnings\\*$")
                                                 (name . "^\\*Shell Command Output\\*$")
                                                 (name . "^\\*Async-native-compile-log\\*$")
                                                 (name . "^\\*straight-")))
                                       ("ediff" (or
                                                 (name . "^\\*ediff.*")
                                                 (name . "^\\*Ediff.*")))
                                       ("dired" (mode . dired-mode))
                                       ("terminal" (or
                                                    (mode . term-mode)
                                                    (mode . shell-mode)
                                                    (mode . eshell-mode)))
                                       ("help" (or
                                                (name . "^\\*Help\\*$")
                                                (name . "^\\*info\\*$")
                                                (name . "^\\*helpful"))))))
  :hook
  (ibuffer-mode-hook . +decide-ibuffer-filter-groups)
  :general
  (+leader-keys
    "b i" '("Open ibuffer" . ibuffer)))


(use-package ibuffer-projectile)

(use-package ibuffer-vc
  :after ibuffer
  :hook
  (ibuffer-mode-hook . ibuffer-vc-set-filter-groups-by-vc-root))


(use-package recentf
  :straight t
  :hook
  (after-init-hook . recentf-mode))


(+leader-keys
  "b r" '("Revert" . revert-buffer)
  "b K" '("Kill this buffer" . kill-current-buffer)
  "b k" '("Kill buffer" . kill-buffer)
  "f o" '("Find file in other window" . find-file-other-window)
  "f R" '("Rename current file" . rename-visited-file)
  "o x" '("Scratch buffer" . scratch-buffer))

(general-def global-map
  "C-x k" 'kill-current-buffer)


(provide 'init-buffers)
