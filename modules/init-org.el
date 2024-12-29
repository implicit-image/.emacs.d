(require 'f)

(setq +modules/org-path (f-join +modules/path "org"))

(add-to-list 'load-path +modules/org-path)


(require 'init-org-mode)

(require 'init-org-babel)

(require 'init-org-agenda)

(require 'init-org-roam)

(provide 'init-org)
