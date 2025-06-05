;;; -*- lexical-binding: t -*-


;; (defun +split-below-window-prefix ()
;;   (interactive)
;;   (display-buffer-override-next-command
;;    (lambda (buffer alist)
;;      (let ((alist (append '((inhibit-same-window . t)
;;                             (window-width . 0.5)
;;                             ()) alist))
;;            window type)
;;        (if (setq ))))))
;;
;; (defun +split-right-window-prefix)
;;
;; (defun +split-left-window-prefix)

(setq window-divider-default-places 'right-only
      window-divider-default-right-width 1
      window-divider-default-bottom-width 1)

(add-hook 'window-setup-hook 'window-divider-mode)

(use-package ace-window
  :commands
  (ace-window)
  :init
  (setq aw-keys '(?a ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?S aw-swap-window "Swap Windows")
          (?m aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?b aw-switch-buffer-in-window "Select Buffer")
          (?F aw-flip-window)
          (?! aw-execute-command-other-window "Execute Command Other Window")
          (?v aw-split-window-vert "Split Vert Window")
          (?s aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?t aw-transpose-frame "Transpose Frame")
          (?? aw-show-dispatch-help)))
  :bind*
  (("C-w w" . ace-window)))

(setq display-buffer-alist '(;; no window
                             ((or . ((derived-mode . calibredb-search-mode)
                                     (derived-mode . calibredb-edit-annotation-mode)
                                     (derived-mode . calibredb-show-mode-hook)))
                              (display-buffer-reuse-window)
                              (window-parameters . ((mode-line-format . none))))
                             ;; flymake
                             ((or ((derived-mode . flymake-project-diagnostics-mode)
                                   (derived-mode . flymake-diagnostics-buffer-mode)))
                              (display-buffer-in-side-window)
                              (side . bottom)
                              (window-height . 0.35)
                              (window-parameters . ((mode-line-format . mone))))
                             ((or . ("\*Warnings\*"))
                              (display-buffer-no-window))
                             ;; bottom side window
                             ((or . ((derived-mode . rg-mode)
                                     (derived-mode . grep-mode)
                                     (derived-mode . xref--xref-buffer-mode)))
                              (display-buffer-same-window)
                              (dedicated . t)
                              (post-command-select-window . t))
                             ((or . ("\*Completions\*"
                                     (derived-mode . completion-list-mode)))
                              (display-buffer-below-selected)
                              (preserve-size . (t . t))
                              (window-parameters . ((mode-line-format . none))))
                             ;; popup bottom buffers
                             ((or . ("\*Org Select\*"
                                     "\*lsp-bridge-doc\*"
                                     "\*lsp-help\*"
                                     "\*tide-documentation\*"
                                     "\*eldoc\*"
                                     (derived-mode . help-mode)
                                     (derived-mode . lsp-ui-imenu-mode)
                                     (derived-mode . apropos-mode)
                                     (derived-mode . helpful-mode)))
                              (display-buffer-below-selected)
                              (window-height . 0.4)
                              (preserve-size . (t . t))
                              (window-parameters . ((mode-line-format . none)))
                              (post-command-select-window . t))
                             ;; shell command
                             ((or . ((derived-mode . shell-command-mode)))
                              (display-buffer-below-selected)
                              (window-height . (body-lines . shrink-window-if-larger-than-buffer)))
                             ;; embark shenanigans
                             ((or . ("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                                     " *Embark Actions*"
                                     (derived-mode . embark-collect-mode)))
                              (display-buffer-reuse-mode-window display-buffer-in-side-window)
                              (side . bottom)
                              (window-height . 17)
                              (window-parameters . ((mode-line-format . none))))
                             ;; interactive bottom buffers
                             ((or . ((derived-mode . flycheck-error-list-mode)
                                     (derived-mode . comint-mode)
                                     (derived-mode . compilation-mode)
                                     (derived-mode . sly-repl-mode)
                                     (derived-mode . lsp-treemacs-error-list-mode)
                                     "\*Dictionary\*"
                                     "\*LSP Lookup\*"
                                     "\*vc-diff\**"))
                              (display-buffer-reuse-mode-window display-buffer-below-selected)
                              (window-height . 0.4)
                              (dedicated . t)
                              (post-command-select-window . t))
                             ;; left sidebar
                             ((and . (" \*Treemacs-*"
                                      (derived-mode . treemacs-mode)))
                              (display-buffer-in-side-window)
                              (dedicated . t)
                              (side . left))
                             ;; right sidebar
                             ((or . ((derived-mode . pdf-outline-buffer-mode)
                                     "\*Call Hierarchy\*"))
                              (display-buffer-in-side-window)
                              (window-parameters . ((mode-line-format . none)))
                              (dedicated . t)
                              (side . right))
                             ;; some window
                             ((or . ("*aider"))
                              (display-buffer-in-side-window)
                              (side . right))
                             ;; top split
                             ;; left split
                             ;; right split
                             ;; temp same window
                             ((or . ("\*Org-Babel\*"
                                     "\*Org Src\*"))
                              (display-buffer-same-window)
                              (dedicated . nil))
                             ;; bottom side window
                             ((or . ((derived-mode . proced-mode)))
                              (display-buffer-at-bottom)
                              (window-height . 15)
                              (dedicated . t))))

(setq switch-to-buffer-in-dedicated-window t
      switch-to-buffer-obey-display-actions nil
      switch-to-buffer-preserve-window-point t)

(use-package windmove
  :straight nil
  :bind*
  ( :map override-global-map
    ("C-w h" . windmove-left)
    ("C-w j" . windmove-down)
    ("C-w k" . windmove-up)
    ("C-w l" . windmove-right)))

(use-package window
  :straight nil
  :bind*
  ( :map override-global-map
    ("C-w -" . shrink-window)
    ("C-w +" . enlarge-window)
    ("C-w q" . delete-window)
    ("C-w v" . split-window-horizontally)
    ("C-w s" . split-window-vertically)
    ("C-w =" . balance-windows)
    ("C-w O" . delete-other-windows)
    ("C-w -" . shrink-window)
    ("C-w +" . enlarge-window)
    ("C-w M--" . shrink-window-horizontally)
    ("C-w M-+" . enlarge-window-horizontally)
    ("M-o" . other-window-prefix)
    ("M-w" . same-window-prefix)))

(provide 'init-windows)
