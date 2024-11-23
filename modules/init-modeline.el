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

(use-package mini-echo
  :demand
  :custom-face
  (minibuffer-prompt ((t (:foreground ,(doom-color 'fg)))))
  (mini-echo-minibuffer-window
   ((t (:background ,(doom-lighten (face-attribute 'default :background nil t) 0.05)))))
  (mini-echo-blue ((t (:foreground ,(doom-color 'blue)))))
  :init
  (require 'mini-echo-segments)
  (setq mode-line-format "")
  (setq mini-echo-right-padding 2
	mini-echo-project-detection 'projectile
	mini-echo-separator "|"
	mini-echo-persistent-function 'ignore
	mini-echo-persistent-rule '(:long
				    ("major-mode" "buffer-name" "project" "evil" "buffer-position" "lsp-bridge" "flycheck" "text-scale")
				    :short
				    ("major-mode" "shrink-path" "evil" "buffer-position" "flycheck")))
  :config
  (mini-echo-mode))

(provide 'init-modeline)
