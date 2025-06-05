;;; -*- lexical-binding: t -*-

(add-hook 'minibuffer-mode-hook 'visual-line-mode)

(setopt read-buffer-completion-ignore-case t
        read-file-name-completion-ignore-case t
        ;; for normal completion mechanism to make sense
        completions-format 'one-column
        completions-max-height 15
        completions-header-format nil
        completions-detailed t
        completions-sort 'historical)

(use-package minibuffer
  :straight nil
  :bind*
  ( :map completion-in-region-mode-map
    ("RET" . minibuffer-choose-completion)
    ("M-n" . minibuffer-next-completion)
    ("M-p" . minibuffer-previous-completion)))

(use-package dabbrev
  :straight nil
  :init
  (setopt dabbrev-upcase-means-case-search t
          dabbrev-ignored-buffer-modes '(archive-mode image-mode docview-mode pdf-view-mode tags-table-mode csv-mode)))

(use-package marginalia
  :custom
  (marginalia-align 'center)
  (marginalia-align-offset 0)
  :hook
  (after-init-hook . marginalia-mode))

(use-package vertico
  :custom
  (vertico-count 13)
  (vertico-resize t)
  (vertico-cycle t)
  (vertico-preselect 'first)
  (vertico-scroll-margin 5)
  (vertico-grid-annotate 0)
  (vertico-grid-max-columns 2)
  (vertico-grid-lookahead 250)
  :hook
  (marginalia-mode-hook . vertico-mode)
  (vertico-mode . vertico-grid-mode)
  :bind*
  (("M-`" . vertico-suspend)
   :map meow-search-global-map
   ("s" . vertico-suspend)
   :map vertico-map
   ("M-j" . vertico-next)
   ("C-J" . vertico-next-group)
   ("M-k" . vertico-previous)
   ("C-K" . vertico-previous-group)
   ("C-c f" . vertico-flat-mode)
   ("C-c s" . vertico-suspend)
   ("C-c ." . vertico-repeat)
   ("C-c i" . vertico-insert)))

(use-package embark-consult)

(use-package embark
  :custom
  (embark-mixed-indicator-delay 0.3)
  (embark-prompter 'embark-keymap-prompter)
  (embark-quit-after-action t)
  (embark-indicators '(embark-which-key-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  :init
  (advice-add 'embark-completing-read-prompter
              :around 'embark-hide-which-key-indicator)
  :bind*
  (("M-." . embark-dwim)
   ("M-'" . embark-act)
   :map vertico-map
   ("C-c C-e" . embark-export)
   ("C-c C-b" . embark-become)
   ("C-c C-l" . embark-live)
   ("C-c C-o" . embark-open-externally)
   ("C-c C-a" . embark-act)
   ("C-c C-d" . embark-dwim)
   ("C-c C-i" . embark-insert)
   ("C-c C-s" . embark-select)
   ("C-c C-c" . embark-collect)))

(use-package consult
  :autoload
  (consult--read)
  :custom
  (xref-show-xrefs-function 'consult-xref)
  :init
  (setq consult-find-args (+os/per-system! :wsl  (format "%s . -not ( -path */.[A-Za-z]* -prune )"
                                                         find-program)
                                           :linux (format "%s . -not ( -path */.[A-Za-z]* -prune )"
                                                          find-program)
                                           :win (format "%s . -not ( -path */.[A-Za-z]* -prune )"
                                                        find-program)))
  :bind*
  ( :map vertico-map
    ("C-c C-h" . consult-history)
    :map project-prefix-map
    ("b" . consult-project-buffer)
    :map vertico-map
    ("M-s s" . consult-isearch-forward)
    ("M-s S-s" . consult-isearch-backward)
    :map meow-insert-global-map
    ("r" . consult-register)
    :map meow-toggle-global-map
    ("m" . consult-minor-mode-menu)
    :map meow-buffer-global-map
    ("b" . consult-buffer)
    ("l" . consult-focus-lines)
    ("L" . consult-keep-lines)
    :map meow-jump-global-map
    ("m" . consult-mark)
    ("M" . consult-global-mark)
    ("b" . consult-bookmark)
    ("\"" . consult-register)
    (";" . consult-goto-line)
    :map meow-search-global-map
    ("i" . consult-imenu)
    ("I" . consult-imenu-multi)
    ("b" . consult-line)
    ("B" . consult-line-multi)
    ("o" . consult-outline)
    ("e" . consult-flymake)
    ("c" . consult-compile-error)
    :map meow-grep-global-map
    ("/" . consult-ripgrep)
    ("f" . consult-find)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))


(provide 'init-completion)
