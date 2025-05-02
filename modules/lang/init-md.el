;;; -*- lexical-binding: t -*-

(use-package markdown-mode
  :init
  (defun +md-check-gfm ()
    (interactive)
    (if (eq )))
  :general
  (gfm-view-mode-map
   :states 'normal
   "q" 'kill-this-buffer))

(provide 'init-md)
