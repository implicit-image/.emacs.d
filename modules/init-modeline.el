;;; -*- lexical-binding: t -*-

(require 'eieio)
(require 'doom-themes)

(use-package evil-anzu
  :demand
  :config (global-anzu-mode +1)
  :general
  (+leader-keys
    "c R" '("Rename" . anzu-query-replace)
    "s r" '("Query replace" . anzu-query-replace)
    "s R" '("Query replace regexp" . anzu-query-replace-regexp)))

(use-package hide-mode-line
  :hook
  ((calibredb-search-mode
    calibredb-edit-annotation-mode
    calibredb-show-mode)
   . hide-mode-line-mode))

(use-package mood-line
  :custom-face
  (mode-line ((t (:background ,(doom-color 'base4)))))
  (mode-line-inactive ((t (:background ,(doom-color 'base3)))))
  (mode-line-active ((t (:background ,(doom-color 'base4)))))
  :demand
  ;; Enable mood-line
  :config
  (setq mood-line-format
	(mood-line-defformat
	 :left
	 (((mood-line-segment-modal) . " ")
	  ((mood-line-segment-anzu) . " ")
	  ((propertize (mood-line-segment-project)
		       'face 'mood-line-status-success)
	   . "/")
	  ((mood-line-segment-buffer-status) . ":")
	  ((mood-line-segment-buffer-name) . " ")
	  ((mood-line-segment-cursor-position) . " ")
	  ((mood-line-segment-multiple-cursors) . ""))
	 :right
	 (((mood-line-segment-misc-info) . " ")
	  ((mood-line-segment-checker) . " ")
	  ((mood-line-segment-vc) . " ")
	  ((mood-line-segment-misc-info) . " ")
	  ((mood-line-segment-major-mode) . " "))))
  (mood-line-mode))

(provide 'init-modeline)
