(use-package exec-path-from-shell
  :demand
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))



(use-package projectile
  :config
  (projectile-mode +1))


(provide 'implicit-projects)
