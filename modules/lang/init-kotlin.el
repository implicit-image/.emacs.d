
(use-package kotlin-mode
  :preface
  (defun +jvm/set-up-kotlin-environment ()
    "Set up kotlin environment variables and locations.")
  :config
  (setq kotlin-tab-width 4)
  :hook
  (kotlin-mode-kotlin . +jvm/set-up-kotlin-environment))

(provide 'init-kotlin)
