;;; -*- lexical-binding: t -*-

(use-package flyspell
  :init
  (setopt flyspell-issue-welcome-flag nil
          flyspell-issue-message-flag nil))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flymake-mode)

(use-package flymake
  :straight nil
  :init
  (setopt flymake-show-diagnostics-at-end-of-line nil
          flymake-start-on-flymake-mode t
          flymake-suppress-zero-counters nil
          flymake-fringe-indicator-position nil
          flymake-margin-indicator-position 'right-margin
          flymake-margin-indicators-string '((error "!" compilation-error)
                                             (warning "$" compilation-warning)
                                             (note "i" compilation-info))
          flymake-mode-line-counter-format '("("
                                             flymake-mode-line-error-counter
                                             flymake-mode-line-warning-counter
                                             flymake-mode-line-note-counter
                                             ")"))

  :bind-keymap*
  ("C-c !" . flymake-prefix-map)
  :bind*
  ( :map flymake-prefix-map
    ("t" . +checkers/flymake-toggle-eol)
    ("]" . flymake-goto-next-error)
    ("[" . flymake-goto-prev-error)
    ("b" . flymake-show-buffer-diagnostics)
    ("p" . flymake-show-project-diagnostics)
    ("l" . flymake-switch-to-log-buffer)))

(provide 'init-checkers)
