;;; -*- lexical-binding: t -*-

(use-package evil-anzu
  :hook
  (after-init . global-anzu-mode)
  :init
  (setq anzu-search-threshold 1500)
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
    fundamental-mode
    help-mode)
   . hide-mode-line-mode))

(use-package mood-line
  :hook
  (window-setup . (lambda ()
                    (interactive)
                    (require 'mood-line)
                    (require 'mood-line-segment-checker)
                    (require 'mood-line-segment-vc)
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
                            ((format-mode-line "L%l:%C") . " ")
                            ((format-mode-line "%o") . " ")
                            ((mood-line-segment-multiple-cursors) . ""))
                           :right
                           (((mood-line-segment-misc-info) . " ")
                            ((mood-line-segment-checker) . " ")
                            ((mood-line-segment-vc) . " ")
                            ((mood-line-segment-major-mode) . " "))))
                    (mood-line-mode 1))))


(provide 'init-modeline)
