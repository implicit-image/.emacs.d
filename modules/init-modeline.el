(require 'eieio)
(require 'doom-themes)

(use-package evil-anzu
  :demand
  :config (global-anzu-mode +1)
  :general
  (+leader-keys
    "c R" '("Rename" . anzu-query-replace)
    "s r" '("Query replace" . anzu-query-replace)
    "s R" '("Query replace regexp" . anzu-query-replace-regexp)))

(use-package hide-mode-line)


(use-package telephone-line
  :demand
  :init
  (setq telephone-line-lhs
	'((evil . (telephone-line-evil-tag-segment))
	  (accent . (telephone-line-buffer-modified-segment))
	  (unimportant . (telephone-line-project-buffer-segment))
	  (nil . (telephone-line-vc-segment)))
	telephone-line-primary-left-separator 'telephone-line-nil
	telephone-line-secondary-left-separator 'telephone-line-nil
	telephone-line-rhs
	'((nil . (telephone-line-flycheck-segment))
	  (evil . (telephone-line-major-mode-segment))
	  (accent . (telephone-line-airline-position-segment)))
	telephone-line-primary-right-separator 'telephone-line-nil
	telephone-line-secondary-right-separator 'telephone-line-nil)

  (setq telephone-line-evil-use-short-tag t
	telephone-line-height 15)
  :config
  (telephone-line-mode 1))

(provide 'init-modeline)
