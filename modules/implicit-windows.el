
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
  (push '("\*Warnings\**"
	  :regexp t
	  :height 0.25
	  :position bottom
	  :dedicated nil) popwin:special-display-config)
  (push '("\*scratch\*"
	  :regexp t
	  :width 0.4
	  :position bottom
	  :dedicated t) popwin:special-display-config)
  (popwin-mode 1))

(provide 'implicit-windows)


