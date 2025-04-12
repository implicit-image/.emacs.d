;;; -*- lexical-binding: t -*-


(use-package company
  :custom-face
  (company-tooltip ((t (:background ,(doom-darken (doom-color 'base4) 0.3)))))
  (company-tooltip-selection ((t (:background ,(doom-color 'bg)))))
  :init
  (setq company-idle-delay nil
        ;; company-backends '((company-dabbrev-code company-keywords)
        ;;                    (company-files company-capf))
        company-backends '(company-capf)
        company-frontends nil
        company-minimum-prefix-length 2
        company-tooltip-scrollbar-width 0
        company-tooltip-limit 15
        company-tooltip-offset-display 'scrollbar
        company-tooltip-maximum-width 200
        company-tooltip-minimum-width 50
        company-tooltip-align-annotations t
        company-quickhelp-delay 0.4
        company-quickhelp-use-propertized-text t
        company-format-margin-function 'company-text-icons-margin)
  :general
  (company-active-map
   :states 'insert
   "C-h" 'company-quickhelp-manual-begin
   "C-s" 'company-search-mode)
  (company-mode-map
   :states '(insert emacs)
   "C-SPC" 'company-complete)
  :hook
  (global-company-mode . company-tng-mode)
  (company-mode . (lambda ()
                    (interactive)
                    (company-quickhelp-local-mode 1)
                    (completion-preview-mode 1)))
  (corfu-mode . (lambda ()
                  (interactive)
                  (when (bound-and-true-p company-mode)
                    (company-mode -1)))))

(provide 'init-company)
