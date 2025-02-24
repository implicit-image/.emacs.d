
(use-package compile
  :straight nil
  :init
  (+windows-cfg '((compilation-mode)
                  :position bottom :height 0.3 :noselect nil :dedicated t))
  :config
  (setq compilation-scroll-output t)
  :hook
  (compilation-mode . visual-line-mode)
  :general
  (+leader-keys
    "c c" '("Compile" . compile)))

(use-package comint
  :straight nil
  :init
  (setq comint-eol-on-send t)
  :general
  (+leader-keys
    "!" '("Comint run" . comint-run)))

(provide 'init-compile)
