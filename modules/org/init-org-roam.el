;;; -*- lexical-binding: t -*-

(defun +org-roam/rg-in-notes ()
  (interactive)
  (consult-ripgrep org-roam-directory ""))


(use-package org-roam
  :custom
  (org-roam-directory (substitute-in-file-name "$HOME/org/roam"))
  (org-roam-dailies-directory "daily")
  (org-roam-dailies-capture-templates '(("d" "default" entry
                                         "* %?"
                                         :target (file+head "%<%Y-%m-%d>.org"
                                                            "#+title: %Y=%m-%d\n"))))
  :init
  (defun +roam/template-fn--file-in-subdir (&optional dir)
    (let* ((root-dir (file-name-as-directory (f-join org-roam-directory
                                                     (if (boundp 'dir)
                                                         (concat dir "/")
                                                       ""))))
           (subdir (read-directory-name "subdirectory: "
                                        root-dir))
           (filename (read-file-name "file: "
                                     (file-name-as-directory subdir))))
      (f-join root-dir
              subdir
              filename)))

  (defun +roam/mode-setup ()
    "Setup `org-roam' related variables and modes."
    (interactive)
    (setq-local org-attach-id-dir (f-join org-roam-directory "data/")))

  (+windows-cfg '(("\*Org Select\*")
                  :position bottom :height 0.3 :noselect nil))
  :config
  (org-roam-db-autosync-mode)
  (require 'org-roam-protocol)
  (require 'org-roam-export)


  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:20}" 'face 'org-tag))
        org-roam-db-node-include-function
        (lambda ()
          (not (member "ATTACH" (org-get-tags))))
        org-roam-db-update-on-save t
        org-roam-completion-everywhere t
        org-roam-capture-templates
        `(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("b" "book notes" plain "%?"
           :if-new (file+head "booknotes/%^{filename}.org" "#+title: %^{title}\n")
           :unnarrowed t
           :jump-to-captured t)
          ("l" "learning" plain "%?"
           :target (file+head "%(+roam/template-fn--file-in-subdir \"/learning\")"
                              "#+title: %^{title}\n")
           :unnarrowed t)))
  :hook
  (org-roam-db-autosync-mode-hook . +roam/mode-setup)
  :general
  (+leader-keys
    "n r" '(:ignore t :which-key "org roam")
    "n r f" '("Find Org Roam node" . org-roam-node-find)
    "n r r" '("Find random Org Roam node" . org-roam-node-random)
    "n r s" '("Sync Org Roam database" . org-roam-db-sync)
    "n r c" '("Org Roam capture" . org-roam-capture)
    "n r *" '("Grep in org roam dir" . +org-roam/rg-in-notes))
  (org-roam-mode-map
   :states '(normal visual)
   :prefix "SPC n r"
   :global-prefix "M-SPC n r"
   "i" '("Insert Org Roam node" . org-roam-node-insert)))

(provide 'init-org-roam)
