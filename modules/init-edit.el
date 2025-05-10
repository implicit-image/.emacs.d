;;; -*- lexical-binding: t -*-

(defun +show-paren--overlay-function (text)
  "Show TEXT in an overlay at the top-left of the current window with nice face spec."
  (setq text (replace-regexp-in-string "\n" " " text))
  (show-paren--delete-context-overlay)
  (let* ((beg (window-start))
         (end (save-excursion
                (goto-char beg)
                (line-end-position))))
    (setq show-paren--context-overlay (make-overlay beg end)))
  (overlay-put show-paren--context-overlay 'display text)
  ;; Use the (default very high) `show-paren-priority' ensuring that
  ;; not other overlays shine through (bug#59527).
  (overlay-put show-paren--context-overlay 'priority
               show-paren-priority)
  (overlay-put show-paren--context-overlay
               'face `(:box
                       ( :line-width (1 . -1)
                         :color ,(face-attribute 'shadow :foreground))
                       :foreground ,(doom-color 'red)))
  (add-hook 'post-command-hook #'show-paren--delete-context-overlay
            nil 'local))


(use-package vundo
  :custom
  (vundo-compact-display t)
  (vundo-window-max-height 6)
  :general
  (global-map
   "C-x u" '("Visualize undo" . vundo)))

(use-package paren
  :straight nil
  :custom
  (show-paren-style 'mixed)
  (show-paren-delay 0.05)
  (show-paren-context-when-offscreen 'overlay)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  :config
  (advice-add 'show-paren--show-context-in-overlay :override '+show-paren--overlay-function)
  :hook
  (after-init-hook . show-paren-mode))


(use-package drag-stuff
  :general
  (global-map
   :states 'visual
   "M-k" 'drag-stuff-up
   "M-j" 'drag-stuff-down))

;; (use-package origami
;;   :straight (origami :type git
;;                      :host github
;;                      :repo "elp-revive/origami.el")
;;   :hook
;;   (after-init-hook . global-origami-mode))

(use-package tabify
  :straight nil
  :general
  (+leader-keys
    "t TAB" 'tabify
    "t <tab>" 'tabify
    "t S-TAB" 'untabify
    "t <backtab>" 'untabify))

(use-package combobulate
  :straight (combobulate :type git
                         :host github
                         :nonrecursive t
                         :repo "mickeynp/combobulate")
  :functions
  (combobulate-mode)
  :custom
  (combobulate-key-prefix "C-c o")
  :hook
  (prog-mode-hook . combobulate-mode))

(use-package vlf)

(provide 'init-edit)
