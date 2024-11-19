
(defun +completion/initialize ()
  "Set up completion functions and load required libraries.")
;; 

(defun +completion/popup ()
  "Show a completion popup.")


(use-package company
  :disabled t)

;; disable loading company if something tries
(defalias 'company-mode 'ignore)


(use-package ivy
  :after marginalia
  :custom-face
  (ivy-current-match ((t (:background ,((doom-lighten (doom-color 'selection) 0.2))))))
  :init
  (setq ivy-height 15
	ivy-fixed-height-minibuffer t
	ivy-use-virtual-buffers t
	enable-recursive-minibuffers t)
  :config
  (ivy-mode)
  :hook (after-init . ivy-mode))

(use-package counsel
  :after evil
  :config
  ;; dont use package.el at all
  (defalias 'counsel-package 'ignore))

(use-package swiper
  :after evil)

(use-package marginalia
  :demand
  :init
  (setq marginalia-align 'center
	marginalia-align-offset -3)
  :config
  (marginalia-mode +1))

;;################################################################
;; Embark + consult + vertico stack
;; disabled for now
;; NOTE: maybe replace ivy+counsel in the future
(use-package embark
  :disabled
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package consult
  :disabled)

(use-package vertico
  :disabled
  :custom
  (vertico-count 13)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode))


(use-package embark-consult
  :disabled
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; #########################################################

;; in-buffer completion

(use-package yasnippet-capf
  :after doom-snippets
  :config
  (add-hook 'completion-at-point-functions #'yasnippet-capf))

(use-package tempel
  :after cape)

(use-package tempel-collection
  :after tempel)


(provide 'implicit-completion)

