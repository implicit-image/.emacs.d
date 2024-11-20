(defun +windows-cfg (&rest popwin-cfg-forms)
  "Each POPWIN-CFG-FORM is (BUFFER-NAMES . POPWIN-OPTIONS-PLIST)."
  (mapc (lambda (cfg-form)
	  (let ((buffers (car cfg-form))
		(cfg-opts (cdr cfg-form)))
            (mapc (lambda (buffer)
		    (add-to-list 'popwin:special-display-config
				 (append (list buffer) cfg-opts)))
		  buffers)))
	popwin-cfg-forms))

(use-package frame
  :straight nil
  :config
  (setq window-divider-default-places t
	window-divider-default-right-width 1
	window-divider-default-bottom-width 1)
  (window-divider-mode)
  :custom-face
  (window-divider ((t (:background "#a1a1a1" :foreground "#a1a1a1")))))

(use-package ace-window
  :after popwin
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package popwin
  :demand
  :config
  (+windows-cfg
   '(("\*Warnings\*" "\*Warnings\**" "\*scratch\*" shell-mode help-mode)
     :regexp t :height 0.3 :position bottom :dedicated nil))
  (popwin-mode 1))

(provide 'implicit-windows)
