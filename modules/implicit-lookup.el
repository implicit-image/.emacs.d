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
(use-package dash-docs)

(use-package helpful
  :config
  ;;popwin support
  (push '("\*helpful*"
	  :regexp t
	  :width 0.2
	  :position bottom
	  :dedicated nil) popwin:special-display-config))


(provide 'implicit-lookup)
