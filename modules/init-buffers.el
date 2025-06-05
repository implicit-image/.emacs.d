;;; -*- lexical-binding: t -*-



(use-package ibuffer
  :straight nil
  :init
  (setopt ibuffer-show-empty-filter-groups nil
          ;; ibuffer-fontification-alist
          ;; '((10 buffer-read-only font-lock-constant-face)
          ;;   (15
          ;;    (and buffer-file-name
          ;;         (string-match ibuffer-compressed-file-name-regexp
          ;;                       buffer-file-name))
          ;;    font-lock-doc-face)
          ;;   (20 (string-match "^\\*" (buffer-name)) font-lock-keyword-face)
          ;;   (25 (and (string-match "^ " (buffer-name)) (null buffer-file-name))
          ;;       italic)
          ;;   (30 (memq major-mode ibuffer-help-buffer-modes)
          ;;       font-lock-comment-face)
          ;;   (35 (derived-mode-p 'dired-mode) font-lock-function-name-face)
          ;;   (40 (and (boundp 'emacs-lock-mode) emacs-lock-mode)
          ;;       ibuffer-locked-buffer))
          ibuffer-saved-filter-groups
          '(("default"
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

(with-eval-after-load 'init-meow
  (bind-keys*
   ("C-x k" . kill-current-buffer)
   ("C-x K" . kill-buffer)
   :map meow-buffer-global-map
   ("k" . kill-buffer)
   ("K" . kill-current-buffer)
   ("r" . revert-buffer)
   ("i" . ibuffer)
   ("x" . scratch-buffer)))

(use-package ibuffer-vc
  :after ibuffer
  :hook
  (ibuffer-mode-hook . ibuffer-vc-set-filter-groups-by-vc-root))

(use-package recentf
  :hook
  (after-init-hook . recentf-mode)
  :bind*
  ( :map meow-file-global-map
    ("r" . recentf)))

(provide 'init-buffers)
