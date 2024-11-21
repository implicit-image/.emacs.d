(defun +org-roam/rg-in-notes ()
  (interactive)
  (counsel-rg nil org-roam-directory))


(use-package org-roam
  :demand
  :custom
  (org-roam-directory (substitute-in-file-name "$HOME/org/roam"))
  (org-roam-dailies-directory "daily")
  (org-roam-dailies-capture-templates '(("d" "default" entry
					 "* %?"
					 :target (file+head "%<%Y-%m-%d>.org"
							    "#+title: %Y=%m-%d\n"))))
  :config
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)
  (require 'org-roam-export)
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag))
        org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags))))
        org-roam-db-update-on-save t
        org-roam-completion-everywhere t))

(provide 'init-org-roam)
