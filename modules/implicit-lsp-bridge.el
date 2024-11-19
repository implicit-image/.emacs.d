

;;;###autoload
;; (defun +lsp/reload-autocompletion ()
;;   ""
;;   (interactive)
;;   (if (display-graphic-p)
;;       (when (featurep 'acm-terminal)
;; 	(acm-terminal-deactive)
;; 	(message "Deactivating acm-terminal")
;; 	(message "Reloading acm")
;; 	(acm-mode -1)
;; 	(acm-mode +1))
;;     (when (not (featurep 'acm-terminal))
;;       (require 'acm-terminal)
;;       (acm-terminal-active)
;;       (message "Activated acm'terminal"))))

;; (add-function :after after-focus-change-function '+lsp/reload-autocompletion)



(use-package lsp-bridge
  :demand
  :straight (lsp-bridge :type git
			:host github
			:repo "manateelazycat/lsp-bridge"
			:files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
			;; do not perform byte compilation or native compilation for lsp-bridge
			:build (:not compile))
  :custom-face
  (lsp-bridge-semantic-tokens-variable-face ((t (:family "Iosevka Comfy" :box (:color "#FFDD33" :line-width -1 :style nil)))))
  :init
  (+windows/cfg
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
  (global-lsp-bridge-mode 1))


(use-package popon)

(use-package acm-terminal
  :custom-face
  (acm-terminal-default-face ((t (:background "#343434"))))
  (acm-terminal-select-face ((t (:background "#111111" :box t))))
  :straight (acm-terminal :host github
			  :repo "twlz0ne/acm-terminal")
  :hook
  (lsp-bridge-mode . (lambda ()
		       (require 'acm-terminal))))


(provide 'implicit-lsp-bridge)





