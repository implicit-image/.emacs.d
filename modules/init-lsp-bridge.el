;;; -*- lexical-binding: t -*-


(use-package lsp-bridge
  ;; if we are on nixos dont install `lsp-bridge' using straight
  ;; :straight `(,@(when (not (executable-find "nixos-rebuild"))
  ;;                 `(lsp-bridge :type git
  ;;                              :host github
  ;;                              :repo "manateelazycat/lsp-bridge"
  ;;                              :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
  ;;                              ;; do not perform byte compilation or native compilation for lsp-bridge
  ;;                              :build (:not compile))))

  :straight nil
  ;; ;; :custom-face
  ;; (lsp-bridge-semantic-tokens-variable-face ((t (:family "Iosevka Comfy" :box (:color "#FFDD33" :line-width -1 :style nil)))))
  :config
  (setq lsp-bridge-enable-log t
        lsp-bridge-complete-manually nil
        lsp-bridge-enable-inlay-hint t
        lsp-bridge-enable-hover-diagnostic t
        lsp-bridge-symbols-enable-which-func t
        lsp-bridge-enable-with-tramp t
        lsp-bridge-enable-diagnostics t
        lsp-bridge-enable-completion-in-minibuffer t
        lsp-bridge-diagnostic-enable-overlays t
        lsp-bridge-org-babel-lang-list nil
        lsp-bridge-enable-signature-help t
        lsp-bridge-signature-show-function #'lsp-bridge-signature-show-with-frame
        lsp-bridge-signature-show-with-frame-position 'point
        lsp-bridge-c-lsp-server "ccls"
        lsp-bridge-nix-lsp-server "nixd"
        lsp-bridge-python-lsp-server "basedpyright")
  :hook
  (lsp-bridge-mode-hook . (lambda ()
                            (interactive)
                            (indent-tabs-mode -1)
                            (apheleia-mode -1))))
;; :general
;; (lsp-bridge-mode-map
;;  :states '(insert)
;;  "C-SPC" 'lsp-bridge-popup-complete-menu)
;; (lsp-bridge-mode-map
;;  :states '(normal visual)
;;  :prefix "SPC c"
;;  :global-prefix "M-SPC c"
;;  "a" '("Code actions" . lsp-bridge-code-action)
;;  "d" '("Find definition" . lsp-bridge-find-def)
;;  "D" '("Find definition other window" . lsp-bridge-find-def-other-window)
;;  "t" '("Find typedef" . lsp-bridge-find-type-def)
;;  "T" '("Find typedef other window" . lsp-bridge-find-type-def-other-window)
;;  "f r" '("Return to symbol" . lsp-bridge-find-def-return)
;;  "i" '("Find implementation" . lsp-bridge-find-impl)
;;  "I" '("Find implementation other window" . lsp-bridge-find-impl-other-window)
;;  "r" '("Find references" . lsp-bridge-find-references)
;;  "k" '("Show doc popup" . lsp-bridge-popup-documentation)
;;  "K" '("Show doc buffer" . lsp-bridge-show-documentation)
;;  "s" '(:ignore t :which-key "Server")
;;  "s R" '("Restart process" . lsp-bridge-restart-process)
;;  "R" '("Rename" . lsp-bridge-rename)
;;  "s S" '("LSP imenu" . lsp-bridge-imenu))
;; (lsp-bridge-mode-map
;;  :states '(normal visual)
;;  :prefix "SPC s"
;;  :global-prefix "M-SPC s"
;;  "S" '("List symbols" . lsp-bridge-workspace-list-symbol-at-point))
;; (lsp-bridge-doc-map
;;  :states '(normal visual)
;;  "q" 'quit-window))

(use-package acm
  :straight nil
  :init
  (setq acm-enable-capf t
        acm-enable-yas t
        acm-enable-path t
        ;; acm-enable-elisp t
        acm-enable-doc t
        acm-enable-org-roam t
        acm-enable-preview t
        acm-backend-lsp-candidate-min-length 2)
  :config
  (add-to-list 'acm-backend-capf-mode-list 'org-mode)
  (add-to-list 'acm-backend-capf-mode-list 'nwscript-mode))
;; :general
;; (acm-mode-map
;;  :states 'insert
;;  "<tab>" 'acm-select-next
;;  "M-j" 'acm-select-next
;;  "<backtab>" 'acm-select-prev
;;  "M-k" 'acm-select-prev
;;  "C-<tab>" 'acm-complete
;;  "C-f" 'acm-filter
;;  "M-d" 'acm-doc-toggle
;;  "<escape>" 'evil-normal-state
;;  "C-SPC" 'acm-hide))

(use-package popon)

;; emacs 31 should  add tty child frames
(when (< (string-to-number emacs-version) 31)
  (use-package acm-terminal
    :custom-face
    (acm-terminal-default-face ((t (:background "#343434"))))
                                        ;(acm-terminal-select-face ((t (:background "#111111" :box t))))
    :straight (acm-terminal :host github
                            :repo "twlz0ne/acm-terminal")
    :hook
    (lsp-bridge-mode-hook . (lambda ()
                              (require 'acm-terminal)))))
;; :general
;; (acm-mode-map
;;  :states 'insert
;;  "<tab>" 'acm-select-next
;;  "TAB" 'acm-select-next
;;  [tab] 'acm-select-next
;;  "<backtab>" 'acm-select-prev
;;  "S-TAB" 'acm-select-prev
;;  [backtab] 'acm-select-prev)))

(provide 'init-lsp-bridge)
