;;; -*- lexical-binding: t -*-


(use-package frame
  :straight nil
  :init
  (setq window-divider-default-places 'right-only
        window-divider-default-right-width 1
        window-divider-default-bottom-width 1)
  :hook
  (window-setup-hook . window-divider-mode))

(use-package ace-window
  :commands
  (ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-alist
        '((?d aw-delete-window "Delete Window")
          (?S aw-swap-window "Swap Windows")
          (?m aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?b aw-switch-buffer-in-window "Select Buffer")
          (?F aw-flip-window)
          (?B aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?! aw-execute-command-other-window "Execute Command Other Window")
          ;; (? aw-split-window-fair "Split Fair Window")
          (?V aw-split-window-vert "Split Vert Window")
          (?H aw-split-window-horz "Split Horz Window")
          (?M delete-other-windows "Delete Other Windows")
          (?T aw-transpose-frame "Transpose Frame")
          (?? aw-show-dispatch-help)))
  :bind*
  ( :map meow-window-keymap
    ("w" . ace-window)))
;; :general
;; (+leader-keys
;;   "w w" '("Switch" . ace-window))
;; (global-map
;;  "<remap> <evil-window-next>" 'ace-window
;;  "C-x w" 'ace-window))


(use-package windmove
  :commands
  (windmove-left windmove-right windmove-up windmove-down)
  :bind*
  (("C-h" . windmove-left)
   ("C-j" . windmove-down)
   ("C-k" . windmove-up)
   ("C-l" . windmove-right)
   :map meow-window-keymap
   ("-" . shrink-window)
   ("+" . enlarge-window)
   ("l" . windmove-right)
   ("h" . windmove-left)
   ("j" . windmove-down)
   ("k" . windmove-up)))


(use-package window
  :straight nil
  :init
  ;; setup meow keymap
  (setq meow-window-keymap (make-keymap))

  (meow-define-state window
    "meow state for window navigation"
    :keymap meow-window-keymap)

  (meow-define-keys 'window
    '("<escape>" . meow-normal-mode))


  (defun +windows-cfg (&rest display-cfg-forms)
    "Each one of POPWIN-CFG-FORMS is (BUFFER-NAMES . POPWIN-OPTIONS-PLIST)."
    (mapc (lambda (cfg-form)
            (let ((buffers (car cfg-form))
                  (cfg-opts (cdr cfg-form)))
              (mapc (lambda (buffer)
                      (add-to-list 'popwin:special-display-config
                                   (append (list buffer) cfg-opts)))
                    buffers)))
          popwin-cfg-forms))

  (setq display-buffer-alist '(;; no window
                               ((or . ((derived-mode . calibredb-search-mode)
                                       (derived-mode . calibredb-edit-annotation-mode)
                                       (derived-mode . calibredb-show-mode-hook)))
                                (display-buffer-reuse-window)
                                (window-parameters . ((mode-line-format . none))))
                               ((or . ("\*Warnings\*"))
                                (display-buffer-no-window))
                               ;; bottom side window
                               ((or . ("\*rg\*"
                                       (derived-mode . xref--xref-buffer-mode)))
                                (display-buffer-same-window)
                                (post-command-select-window . t))
                               ;; popup bottom buffers
                               ((or . ("\*Org Select\*"
                                       "\*lsp-bridge-doc\*"
                                       "\*lsp-help\*"
                                       "\*tide-documentation\*"
                                       "\*eldoc\*"
                                       (derived-mode . shell-command-mode)
                                       (derived-mode . help-mode)
                                       (derived-mode . lsp-ui-imenu-mode)
                                       (derived-mode . apropos-mode)
                                       (derived-mode . helpful-mode)))
                                (display-buffer-below-selected)
                                (window-height . 0.4)
                                (preserve-size . (t . t))
                                (window-parameters . ((mode-line-format . none)))
                                (post-command-select-window . t))
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
  :bind-keymap*
  ("C-w" . meow-window-keymap)
  :bind
  ( :map meow-window-keymap
    ("v" . split-window-horizontally)
    ("s" . split-window-vertically)))
;; :general
;; (general-override-mode-map
;;  :states 'normal
;;  "C-M-v" 'scroll-other-window
;;  "M-w" 'same-window-prefix
;;  "M-o" 'other-window-prefix))

(use-package init-wm
  :straight nil)

(provide 'init-windows)
