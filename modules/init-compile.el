(use-package compile
  :straight nil
  :custom
  (compilation-scroll-output t)
  :init
  (defun +compile/open-compile-buffer ()
    "Open *compilation* buffer."
    (interactive)
    (switch-to-buffer "*compilation*" nil t))
  :hook
  (compilation-mode-hook . visual-line-mode))
;; :bind*
;; (("C-x C-c c" . 'compile)))
;; :general
;; (+leader-keys
;;   "c c" '("Compile" . compile)
;;   "c b" '("Open compilation buffer" . +compile/open-compile-buffer)))

(use-package ansi-color
  :straight nil
  :hook
  (compilation-filter-hook 'ansi-color-compilation-filter))

(use-package comint
  :straight nil
  :custom
  (comint-eol-on-send t)
  :bind*
  (("C-x !" . 'comint-run)))
;; :general
;; (comint-mode-map
;;  :states 'normal
;;  "q" 'quit-window)
;; (+leader-keys
;;   "!" '("Comint run" . comint-run)))

(provide 'init-compile)
