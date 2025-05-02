;;; -*- lexical-binding: t -*-

(+module/declare! org)

(require 'init-org-mode)

(require 'init-org-babel)

(require 'init-org-agenda)

(require 'init-org-roam)


;;; local storage management for org mode
;; (use-package org-local-store
;;   :straight nil
;;   :init
;;   (add-to-list 'load-path (expand-file-name "~/projects/org-local-store"))
;;   :hook
;;   (org-mode . org-local-store-mode))

(provide 'init-org)
