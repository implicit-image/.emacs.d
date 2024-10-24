
;; display search/replace candidate count on modeline
(use-package anzu
  :demand
  :after doom-modeline)

(use-package evil-anzu
  :after anzu
  :config (global-anzu-mode +1))


;; the modeline
(use-package doom-modeline
  :demand
  :init
  (setq doom-modeline-buffer-file-name-style 'truncate-nil
        doom-modeline-minor-modes nil
	doom-modeline-height 25
	doom-modeline-project-detection 'auto)
  :config
  (doom-modeline-mode 1))
  


(provide 'implicit-modeline)

