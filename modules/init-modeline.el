;;; -*- lexical-binding: t -*-

(use-package evil-anzu
  :config
  (global-anzu-mode +1)
  :general
  (+leader-keys
    "c R" '("Rename" . anzu-query-replace)
    "s r" '("Query replace" . anzu-query-replace)
    "s R" '("Query replace regexp" . anzu-query-replace-regexp)))

(use-package hide-mode-line
  :hook
  ((calibredb-search-mode
    calibredb-edit-annotation-mode
    calibredb-show-mode
    helpful-mode
    help-mode)
   . hide-mode-line-mode))

(use-package mood-line
  :demand
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
	  ((format-mode-line "L%l") . " ")
	  ((format-mode-line "%o") . " ")
	  ((mood-line-segment-multiple-cursors) . ""))
	 :right
	 (((mood-line-segment-misc-info) . " ")
	  ((mood-line-segment-checker) . " ")
	  ((mood-line-segment-vc) . " ")
	  ((mood-line-segment-major-mode) . " "))))
  (mood-line-mode))


;; (setq-default mode-line-format
;; 	      `("%e"
;; 		;; ,+modeline/evil-state
;; 		" evil "
;; 		;; ,+modeline/project
;; 		(:eval (or (projectile-project-root)
;; 			   (project-root)
;; 			   (vc-root-dir)
;; 			   default-directory))
;; 		;; buffer name
;; 		" %b "
;; 		;; line ending and encoding
;; 		" %Z "
;; 		;; point position
;; 		" %l : %o : %I"
;; 		;; checkers info
;; 		;; vc info
;; 		;; major-mode
;; 		))


(provide 'init-modeline)
