(use-package compile
  :straight nil


  :init
  (defun +compile/open-compile-buffer ()
    "Open *compilation* buffer."
    (interactive)
    (switch-to-buffer "*compilation*" nil t))

  :config
  (setq compilation-scroll-output t)
  :hook
  (compilation-mode-hook . visual-line-mode)
  :general
  (+leader-keys
    "c c" '("Compile" . compile)
    "c b" '("Open compilation buffer" . +compile/open-compile-buffer)))

(use-package comint
  :straight nil
  :init
  (setq comint-eol-on-send t)
  :general
  (comint-mode-map
   :states 'normal
   "q" 'quit-window)
  (+leader-keys
    "!" '("Comint run" . comint-run)))

(provide 'init-compile)
