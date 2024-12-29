(use-package groovy-mode
  :preface
  (defun +jvm/set-up-groovy-environment ()
    "Set up groovy environment variables and locations.")
  :hook
  (groovy-mode . +jvm/set-up-groovy-environment))

(provide 'init-groovy)
