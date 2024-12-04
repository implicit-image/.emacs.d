;;; -*- lexical-binding: t -*-

(use-package markdown-mode
  :general
  (gfm-view-mode-map
   :states 'normal
   "q" 'kill-this-buffer))

(provide 'init-md)
