(require 'eieio)
(require 'doom-themes)

(use-package evil-anzu
  :demand
  :config (global-anzu-mode +1))

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
  ;; (defun +modeline--get-pdf-pos ()
  ;;   "Get (current page . page count) in pdf-view buffer"
  ;;   (let ((max-pages (or (pdf-cache-number-of-pages)
  ;; 			 (pdf-info-number-of-pages (current-buffer))))
  ;; 	  (curr-page (pdf-view-current-page)))
  ;;     (format "%s / %s" curr-page max-pages)))
  ;; (mini-echo-define-segment "pdf-pages"
  ;;   "Show what page the pdf is on and max pages."
  ;;   :
  ;;   :update-hook '(pdf-view-change-page-hook)
  ;;   :update-advice '(())
  ;;   :update (if (bound-and-true-p 'pdf-view-mode)
  ;; 		()))
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
