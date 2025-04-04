;;; -*- lexical-binding: t -*-

(use-package minibuffer
  :straight nil
  :hook
  (minibuffer-mode . visual-line-mode))

(use-package completion-preview
  :straight nil
  :general
  (completion-preview-active-mode-map
   :states '(insert emacs)
   "C-n" 'completion-preview-next-candidate
   "C-p" 'completion-preview-prev-candidate))

(use-package marginalia
  :init
  (setq marginalia-align 'center
        marginalia-align-offset -3)
  :hook
  (after-init . marginalia-mode))

(use-package vertico
  :custom
  (vertico-count 15)
  (vertico-resize nil)
  (vertico-cycle t)
  (vertico-preselect 'first)
  :init
  (setq vertico-scroll-margin 5)
  :hook
  (marginalia-mode . vertico-mode)
  :general
  (+leader-keys
    "t c" '("Bring up last completion" . vertico-suspend))
  (vertico-map
   "C-c ." '("Repeat last vertico session" . vertico-repeat)
   "C-c i" '("Insert candidate" . vertico-insert)
   "C-c s" '("Suspend current session" . vertico-suspend)
   "C-c b" '("Toggle vertico-buffer-mode" . vertico-buffer-mode)))


(use-package embark
  :custom
  (embark-mixed-indicator-delay 0.3)
  :init
  (+windows-cfg '(("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*" " *Embark Actions*" embark-collect-mode)
                  :regexp t
                  :position bottom
                  :height 0.3
                  :dedicated nil
                  :noselect nil))
  (setq prefix-help-command #'embark-prefix-help-command)
  :general
  (+leader-keys
    "e" '("Embark Act" . embark-act)))

(use-package consult
  :autoload
  (consult--read)
  :custom
  (xref-show-xrefs-function 'consult-xref)
  :config

  (defvar +completion/consult-prev-plist nil
    "alist of last recorder picker queries.")

  ;; (defvar +completion/last-consult nil
  ;;   "Last consult query.")

  ;; (defun +completion/save-consult-query (table &rest args)
  ;;   "Save consult info."
  ;;   (interactive)
  ;;   (plist-put +completion/consult-prev-plist
  ;;              (plist-get arg :category))
  ;;   (setq +completion/last-consult (plist-get args :category)
  ;;         +completion/last-consult-list table))

  ;; (defun +completion/show-last-consult ()
  ;;   "Bring up last consult query."
  ;;   (interactive)
  ;;   )

  ;; (advice-add 'consult--read :before '+completion/save-consult-query)

  (consult-customize
   consult-grep consult-ripgrep consult-git-grep consult-line-multi
   :initial "")
  :general
  (org-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "s i" '("Org heading" . consult-org-heading)
   "s o" '("Outline" . consult-outline))
  (markdown-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "s o" '("Outline" . consult-outline))
  (+leader-keys
    "m :" '("Run active mode command" . consult-mode-command)
    "/" '("Grep directory" . consult-ripgrep)
    ;; buffer
    "b b" '("Find buffer" . consult-buffer)
    "c e" '("Compile errors" . consult-compile-error)
    ;; file
    "f r" '("Recent files" . consult-recent-file)
    "f /" '("Find files" . consult-find)
    "h i" '("Emacs Info" . consult-info)
    "o b" '("Bookmarks" . consult-bookmark)
    ;; search
    "s b" '("Search buffer" . consult-line)
    "s B" '("Search all buffers" . consult-line-multi)
    "t m" '("Toggle minor mode" . consult-minor-mode-menu))
  (prog-mode-map
   :states '(normal visual)
   :prefix "SPC"
   :global-prefix "M-SPC"
   "s i" '("Imenu" . consult-imenu)
   "s I" '("imenu everywhere" . consult-imenu-multi)))

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic)))

(+leader-keys
  ":" '("Execute command" . execute-extended-command)
  "h l" '("Load library" . load-library))

(provide 'init-completion)
