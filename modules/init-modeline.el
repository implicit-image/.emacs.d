;;; -*- lexical-binding: t -*-


(use-package hide-mode-line
  :hook
  ((calibredb-search-mode-hook
    calibredb-edit-annotation-mode-hook
    calibredb-show-mode-hook
    helpful-mode-hook
    fundamental-mode-hook
    help-mode-hook)
   . hide-mode-line-mode))

;; (use-package emacs
;;   (setq-default mode-line-format
;;                 ("%e" mode-line-front-space
;;                  (:propertize
;;                   ("" mode-line-mule-info mode-line-client mode-line-modified
;;                    mode-line-remote mode-line-window-dedicated)
;;                   display (min-width (6.0)))
;;                  mode-line-frame-identification mode-line-buffer-identification "   "
;;                  mode-line-position evil-mode-line-tag
;;                  (project-mode-line project-mode-line-format) (vc-mode vc-mode) "  "
;;                  mode-line-modes mode-line-misc-info mode-line-end-spaces)
;;                 ))

(use-package mood-line
  :custom-face
  (mood-line-unimportant ((t (:foreground ,(doom-color 'base6)))))
  :init
  (defun +mood-line-evil-mc-segment ()
    ""
    (when (> (evil-mc-get-cursor-count) 1)
      (evil-mc-active-mode-line nil)))

  (defun +mood-line--setup ()
    (require 'mood-line)
    (require 'mood-line-segment-checker)
    (require 'mood-line-segment-vc)
    (require 'mood-line-segment-indentation)
    (setq-default mood-line-format
                  (mood-line-defformat
                   :left
                   (((mood-line-segment-modal) . " ")
                    ((mood-line-segment-region) . " ")
                    ((+mood-line-evil-mc-segment) . " ")
                    ((mood-line-segment-anzu) . " ")
                    ((propertize (mood-line-segment-project)
                                 'face 'mood-line-status-success)
                     . "/")
                    ((mood-line-segment-buffer-status) . ":")
                    ((mood-line-segment-buffer-name) . " ")
                    ((format-mode-line "L%l:%C") . " ")
                    ((format-mode-line "%o") . " "))
                   :right
                   (((mood-line-segment-misc-info) . " ")
                    ((mood-line-segment-checker) . " ")
                    ((mood-line-segment-vc) . " ")
                    ((mood-line-segment-major-mode) . " "))))
    (mood-line-mode 1))
  :hook
  (after-init-hook . +mood-line--setup))


(provide 'init-modeline)
