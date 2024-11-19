(require 'doom-themes)


(use-package company
  :preface
  ;; disable company mode
  (defalias 'company-mode 'ignore))


(defun +eye-candy/get-doom-face-attribute (face attr)
  "Get an ATTR of a FACE in `doom-themes--faces'. Returns nil if there is no such FACE in `doom-themes--faces'"
(plist-get (cdr (car (seq-filter
 (lambda (face-info)
   (equal (car face-info) test-face))
 doom-themes--faces))) :background))


(use-package ivy
  :demand
  :after doom-themes
  ;; :custom-face
  ;; (ivy-current-match ((t (:background ,(+eye-candy/get-doom-face-attribute 'highlight :background)))))
  :init
  (setq ivy-height 10
	ivy-fixed-height-minibuffer t
	ivy-use-virtual-buffers t
	enable-recursive-minibuffers t)
  :config
  ;; override default initial input
  (ivy-mode)
  (ivy-configure 'counsel-M-x :initial-input "")
  :hook (after-init . ivy-mode))

(use-package counsel
  :after evil)
  
(use-package swiper
  :after evil)

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;################################################################
;; Embark + consult + vertico stack
;; disabled for now
;; NOTE: maybe replace ivy+counsel in the future
(use-package embark
  :disabled
  :bind
  (("C-."   . embark-act)         ;; pick some comfortable binding
   ("C-;"   . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
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
  :general
  (:keymaps 'vertico-map
   "<tab>" #'vertico-insert  ; Insert selected candidate into text area
   "<escape>" #'minibuffer-keyboard-quit ; Close minibuffer
   "C-M-n" #'vertico-next-group
   "C-M-p" #'vertico-previous-group)
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

