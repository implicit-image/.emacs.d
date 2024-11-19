(require 'general)

;; based on DOOM's implementation
;; TODO: tidy  this up
(defun +lookup/documentation (identifier &optional args)
  "Lookup documentation for IDENTIFIER."
  )

(defvar +lookup/documentation-fn nil
  "Local documentation function.")


(use-package dumb-jump
  :init
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package ivy-xref
  :after ivy
  :init
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;; TODO: add :dash to use-package language mode declarations
(use-package dash-docs
  :demand)

(use-package info
  :straight nil
  :init
  (evil-set-initial-state 'Info-mode 'normal)
  :hook (Info-mode . (lambda ()
		       (interactive)
		       (display-line-numbers-mode -1))))

(use-package helpful
  :init
  (+lookup/set-fn 'buffer '((helpful-mode . helpful-at-point)))
  ;;popwin support
  (+windows/cfg
   '(("\*helpful*")
     :regexp t :height 0.3 :position bottom :dedicated nil :stick nil :noselect nil))
  :hook
  (helpful-mode . (lambda () (display-line-numbers-mode -1))))


(use-package dictionary
  :straight nil
  :init
  (+windows/cfg '(("\*Dictionary\*")
		  :position bottom :height 0.3))
  (setq dictionary-server "dict.org")
  :hook
  (dictionary-mode . (lambda ()
		       (display-line-numbers-mode -1))))

(provide 'implicit-lookup)
