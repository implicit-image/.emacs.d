;; (use-package compile
;;   :straight nil
;;   :custom
;;   (compilation-scroll-output t)
;;   :init
;;   (defun +compile/open-compile-buffer ()
;;     "Open *compilation* buffer."
;;     (interactive)
;;     (switch-to-buffer "*compilation*" nil t))
;;   :hook
;;   (compilation-mode-hook . visual-line-mode))

(setopt compilation-scroll-output t)

(add-hook 'compilation-mode-hook 'visual-line-mode)
;; :bind*
;; (("C-c <space> c" . compile)
;;  ("C-x <space> c c" . compile)))
;; :general
;; (+leader-keys
;;   "c c" '("Compile" . compile)
;;   "c b" '("Open compilation buffer" . +compile/open-compile-buffer)))

;; (use-package ansi-color
;;   :straight nil
;;   :hook
;;   (compilation-filter-hook 'ansi-color-compilation-filter))

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(setopt comint-eol-on-send t)

;; (use-package comint
;;   :straight nil
;;   :custom
;;   (comint-eol-on-send t))
;; :bind*
;; (("C-x !" . comint-run)
;;  ("C-x <space> !" . comint-run)))
;; :general
;; (comint-mode-map
;;  :states 'normal
;;  "q" 'quit-window)
;; (+leader-keys
;;   "!" '("Comint run" . comint-run)))

(provide 'init-compile)
