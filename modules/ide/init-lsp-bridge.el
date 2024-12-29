;;; -*- lexical-binding: t -*-

(use-package lsp-bridge
  :straight (lsp-bridge :type git
			:host github
			:repo "manateelazycat/lsp-bridge"
			:files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
			;; do not perform byte compilation or native compilation for lsp-bridge
			:build (:not compile))
  ;; :custom-face
  ;; (lsp-bridge-semantic-tokens-variable-face ((t (:family "Iosevka Comfy" :box (:color "#FFDD33" :line-width -1 :style nil)))))
  :init
  (+windows-cfg
   '(("\*lsp-bridge-doc\*")
     :regexp t :height 0.3 :position bottom :dedicated t :noselect nil))
  :config
  (setq lsp-bridge-enable-log t
	lsp-bridge-complete-manually nil
	lsp-bridge-enable-inlay-hint t
	lsp-bridge-enable-hover-diagnostic t
	lsp-bridge-enable-diagnostics t
	lsp-bridge-enable-completion-in-minibuffer t
	lsp-bridge-diagnostic-enable-overlays t
	lsp-bridge-signature-show-function #'lsp-bridge-signature-show-with-frame
	lsp-bridge-c-lsp-server "ccls"
	acm-enable-icon t
	acm-enable-yas t
	acm-enable-tempel t
	acm-enable-path t
	acm-enable-capf t
	acm-terminal-doc-max-width 50
	acm-terminal-max-width 30)
  :general
  (acm-mode-map
   :states 'insert
   "<tab>" 'acm-select-next
   "M-j" 'acm-select-next
   "<backtab>" 'acm-select-prev
   "M-k" 'acm-select-prev
   "C-<tab>" 'acm-complete
   "C-f" 'acm-filter
   "M-d" 'acm-doc-toggle
   "<escape>" 'acm-hide)
  (lsp-bridge-mode-map
   :states '(insert)
   "C-SPC" 'lsp-bridge-popup-complete-menu)
  (lsp-bridge-mode-map
   :states '(normal visual)
   :prefix "SPC c"
   :global-prefix "M-SPC c"
   "a" '("Code actions" . lsp-bridge-code-action)
   "d" '("Find definition" . lsp-bridge-find-def)
   "D" '("Find definition other window" . lsp-bridge-find-def-other-window)
   "t" '("Find typedef" . lsp-bridge-find-type-def)
   "T" '("Find typedef other window" . lsp-bridge-find-type-def-other-window)
   "f r" '("Return to symbol" . lsp-bridge-find-def-return)
   "i" '("Find implementation" . lsp-bridge-find-impl)
   "I" '("Find implementation other window" . lsp-bridge-find-impl-other-window)
   "r" '("Find references" . lsp-bridge-find-references)
   "k" '("Show doc popup" . lsp-bridge-popup-documentation)
   "K" '("Show doc buffer" . lsp-bridge-show-documentation)
   "s" '(:ignore t :which-key "Server")
   "s R" '("Restart process" . lsp-bridge-restart-process)
   "R" '("Rename" . lsp-bridge-rename)
   "s S" '("LSP imenu" . lsp-bridge-imenu))
  (lsp-bridge-mode-map
   :states '(normal visual)
   :prefix "SPC s"
   :global-prefix "M-SPC s"
   "S" '("List symbols" . lsp-bridge-workspace-list-symbol-at-point))
  (lsp-bridge-doc-map
   :states '(normal visual)
   "q" 'quit-window))


(use-package popon)

;; emacs 31 should  add tty child frames
(when (< (string-to-number emacs-version) 31)
  (use-package acm-terminal
    :custom-face
    (acm-terminal-default-face ((t (:background "#343434"))))
    (acm-terminal-select-face ((t (:background "#111111" :box t))))
    :straight (acm-terminal :host github
			    :repo "twlz0ne/acm-terminal")
    :hook
    (lsp-bridge-mode . (lambda ()
			 (require 'acm-terminal)))))

(provide 'init-lsp-bridge)
