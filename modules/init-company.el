;;; -*- lexical-binding: t -*-


(use-package company
  :init
  (setq company-idle-delay nil
        company-backends '(company-cmake company-capf company-clang company-files
                                         (company-dabbrev-code company-gtags company-etags company-keywords)
                                         company-dabbrev)
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
   "C-h" 'company-quickhelp-manual-begin)
  (company-mode-map
   :states '(insert emacs)
   "C-SPC" 'company-complete)
  :hook
  (after-init . global-company-mode)
  (global-company-mode . company-quickhelp-mode)
  (global-company-mode . company-tng-mode)
  (company-mode . (lambda ()
                    (interactive)
                    (completion-preview-mode 1)))
  (corfu-mode . (lambda ()
                  (interactive)
                  (when company-mode
                    (company-mode -1)))))


(provide 'init-company)
