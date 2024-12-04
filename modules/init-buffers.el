;;; -*- lexical-binding: t -*-

(defun +buffers/decide-ibuffer-filters ()
  (interactive)
  (let ((group (symbol-name 'default)))
    (ibuffer-switch-to-saved-filter-groups group)))


;; add default config filenames to correct automodes

(defvar +buffers/special-auto-mode-alist ()
  "Alist of special file names and corresponding major modes.")



(use-package ibuffer
  :straight nil
  :init
  (setq ibuffer-show-empty-filter-groups nil)
  :hook (ibuffer-mode . #'+buffers/decide-ibuffer-filters)
  :general
  (+leader-keys
    "b i" '("Open ibuffer" . ibuffer)))


(use-package ibuffer-projectile)

(use-package ibuffer-vc
  :after ibuffer
  :hook
  (ibuffer-mode . ibuffer-vc-set-filter-groups-by-vc-root))


(use-package recentf
  :straight t
  :demand
  :config
  (recentf-mode 1))


(+leader-keys
  "b r" '("Revert" . revert-buffer)
  "b K" '("Kill this buffer" . kill-this-buffer)
  "b k" '("Kill buffer" . kill-buffer)
  "f o" '("Find file in other window" . find-file-other-window)
  "f R" '("Rename current file" . rename-visited-file)
  "o x" '("Scratch buffer" . scratch-buffer))

(general-def global-map
    "C-=" 'text-scale-increase
    "C--" 'text-scale-decrease
    "C-x k" 'kill-this-buffer)


(provide 'init-buffers)
