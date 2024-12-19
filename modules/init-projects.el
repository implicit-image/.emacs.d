;;; -*- lexical-binding: t -*-

(use-package projectile
  :config
  (projectile-mode +1)
  :general
  (+leader-keys
    "SPC" '("Find file in project" . projectile-find-file)
    "p !" '("Run cmd in project root"     . projectile-run-shell-command-in-root)
    "p &" '("Async cmd in project root"   . projectile-run-async-shell-command-in-root)
    "p ." '("Browse project"              . projectile-find-file)
    "p >" '("Browse other project"        . projectile-find-other-file)
    "p a" '("Add new project"             . projectile-add-known-project)
    "p b" '("Switch to project buffer"    . projectile-switch-to-buffer)
    "p c" '("Compile in project"          . projectile-compile-project)
    "p C" '("Repeat last command"         . projectile-repeat-last-command)
    "p d" '("Remove known project"        . projectile-remove-known-project)
    "p D" '("Discover projects in folder" . projectile-discover-projects-in-directory)
    "p e" '("Edit project's .dir-locals"  . projectile-edit-dir-locals)
    "p f" '("Find file in project"        . projectile-find-file)
    "p F" '("Find file in other project"  . projectile-find-other-file)
    "p g" '("Configure project"           . projectile-configure-project)
    "p i" '("Invalidate project cache"    . projectile-invalidate-cache)
    "p k" '("Kill project buffers"        . projectile-kill-buffers)
    "p o" '("Find sibling file"           . projectile-find-related-file)
    "p p" '("Switch project"              . projectile-switch-project)
    "p r" '("Find recent project files"   . projectile-recentf-files)
    "p R" '("Run project"                 . projectile-run-project)
    "p s" '("Save project files"          . projectile-save-project-buffers)
    "p T" '("Project terminal"            . projectile-run-vterm))
  (global-map
   "C-c p" '("Projectile commands" . projectile-command-map)))

(use-package persp-mode
  :config
  (setq persp-autokill-buffer-on-remove 'kill-weak
	persp-reset-windows-on-nil-window-conf
	persp-nil-hidden t))

(use-package desktop
  :straight nil
  :init
  (setq desktop-restore-frames t
	desktop-restore-eager t
	desktop-restore-reuses-frames t)
  :hook
  (after-init . desktop-save-mode))

(provide 'init-projects)
