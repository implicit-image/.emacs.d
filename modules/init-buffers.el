;;; -*- lexical-binding: t -*-

(use-package ibuffer
  :straight nil
  :custom
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-saved-filter-groups '(("default"
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
                                           (name . "^\\*helpful")))))))
;; :bind*
;; (("C-x C-b i" . ibuffer)))
;; :general
;; (+leader-keys
;;   "b i" '("Open ibuffer" . ibuffer)))


(use-package ibuffer-projectile)

(use-package ibuffer-vc
  :after ibuffer
  :hook
  (ibuffer-mode-hook . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package recentf
  :straight t
  :hook
  (after-init-hook . recentf-mode))

;; (+leader-keys
;;   "b r" '("Revert" . revert-buffer)
;;   "b K" '("Kill this buffer" . kill-current-buffer)
;;   "b k" '("Kill buffer" . kill-buffer)
;;   "f o" '("Find file in other window" . find-file-other-window)
;;   "f R" '("Rename current file" . rename-visited-file)
;;   "o x" '("Scratch buffer" . scratch-buffer))
;;
;; (general-def global-map
;;   "C-x k" 'kill-current-buffer)


(provide 'init-buffers)
