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
  (vundo-window-max-height 6))
;; :bind*
;; (("C-x u" . vundo)))

;; (use-package paren
;;   :straight nil
;;   :custom
;;   (show-paren-style 'mixed)
;;   (show-paren-delay 0.05)
;;   (show-paren-context-when-offscreen 'overlay)
;;   (show-paren-when-point-inside-paren t)
;;   (show-paren-when-point-in-periphery t)
;;   :config
;;   (advice-add 'show-paren--show-context-in-overlay :override '+show-paren--overlay-function)
;;   :hook
;;   (after-init-hook . show-paren-mode))

(setopt show-paren-style 'mixed
        show-paren-delay 0.05
        show-paren-context-when-offscreen 'overlay
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery t)

(with-eval-after-load 'paren
  (advice-add 'show-paren--show-context-in-overlay :override '+show-paren--overlay-function))

(add-hook 'after-init-hook 'show-paren-mode)


(use-package drag-stuff)
;; :bind*
;; (("M-k" . drag-stuff-up)
;;  ("M-j" . drag-stuff-down)))

;; (use-package tabify
;;   :straight nil)
;; :bind*
;; (("C-x SPC t TAB" . tabify)
;;  ("C-x SPC t <tab>" . tabify)
;;  ("C-x SPC t S-TAB" . untabify)
;;  ("C-x SPC t <backtab>" . untabify)))

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

(setq treesit-font-lock-level 3)

(use-package treesit-auto
  :commands
  (global-treesit-auto-mode)
  :init
  (setq treesit-auto-install t)
  :hook
  (after-init-hook . (lambda ()
                       (require 'treesit-auto)
                       (treesit-auto-add-to-auto-mode-alist 'all)
                       (global-treesit-auto-mode))))


(provide 'init-edit)
