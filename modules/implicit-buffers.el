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
  :hook (ibuffer-mode . #'+buffers/decide-ibuffer-filters))


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


(provide 'implicit-buffers)
