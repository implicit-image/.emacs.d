(require 'doom-themes)

;;;###autoloads
(defun +appearance-setup-doom-themes ()
  (progn
    (add-to-list 'custom-theme-load-path (file-name-concat straight-base-dir
                                                           "straight"
                                                           straight-build-dir
                                                           "doom-gruber-darker-theme/"))
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t
          doom-themes-treemacs-enable-variable-pitch nil
          doom-themes-treemacs-theme "doom-colors")
    (doom-themes-visual-bell-config)
    (load-theme +base/theme t)))


(provide 'init-appearance-autoloads)
