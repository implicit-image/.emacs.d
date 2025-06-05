;;; -*- lexical-binding: t -*-

(use-package simple
  :straight nil
  :init
  (setopt next-error-recenter 4
          next-error-highlight t
          next-error-highlight-no-select t))

(use-package comint
  :straight nil
  :init
  (setopt comint-eol-on-send t
          comint-prompt-read-only t))

(use-package compile
  :straight nil
  :init
  (setopt compilation-scroll-output t
          compilation-auto-jump-to-first-error t
          compilation-max-output-line-length 500
          compilation-search-all-directories t
          compilation-skip-threshold 0)
  :hook
  (compilation-filter-hook . ansi-color-compilation-filter))

(with-eval-after-load 'meow
  (bind-keys*
   ("C-x c" . compile)
   ("C-x C-!" . comint-run)))

(use-package dape
  :init
  (setq dape-key-prefix (kbd "C-c D"))
  :config
  (setq dape-buffer-window-arrangement 'right))

(provide 'init-compile)
