;;; -*- lexical-binding: t -*-

(use-package evil-anzu
  :init
  (setq anzu-search-threshold nil
        anzu-replace-threshold nil)
  :hook
  (after-init-hook . global-anzu-mode)
  :general
  (+leader-keys
    "c R" '("Rename" . anzu-query-replace)
    "s r" '("Query replace" . anzu-query-replace)
    "s R" '("Replace at point" . anzu-replace-threshold)
    "s R" '("Query replace regexp" . anzu-query-replace-regexp)))

(use-package hide-mode-line
  :hook
  ((calibredb-search-mode-hook
    calibredb-edit-annotation-mode-hook
    calibredb-show-mode-hook
    helpful-mode-hook
    fundamental-mode-hook
    help-mode-hook)
   . hide-mode-line-mode))

(use-package mood-line
  :custom-face
  (mood-line-unimportant ((t (:foreground ,(doom-color 'base6)))))
  :init
  (defun +mood-line-evil-mc-segment ()
    ""
    (when (> (evil-mc-get-cursor-count) 1)
      (evil-mc-active-mode-line nil)))
  :hook
  (window-setup-hook . (lambda ()
                         (interactive)
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
                         (mood-line-mode 1))))


(provide 'init-modeline)
