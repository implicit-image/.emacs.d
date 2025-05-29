;;; -*- lexical-binding: t -* t t t-


(use-package sideline-flycheck
  ;; :custom-face
  ;; (sideline-flycheck-info ((t (:background ,(doom-color 'base2)))))
  ;; (sideline-flycheck-warning ((t (:background ,(doom-color 'base2)))))
  ;; (sideline-flycheck-error ((t (:background ,(doom-color 'base2)))))
  :init
  (setq sideline-flycheck-max-lines 5
        sideline-flycheck-show-checker-name nil))

(use-package sideline-blame
  ;; :custom-face
  ;; (sideline-blame ((t (:box (:line-width -1) :background ,(doom-color 'base3) :slant normal :size ,+base/font-size))))
  :init
  (setq sideline-blame-commit-format "* %s"))

(use-package sideline-lsp
  ;; :custom-face
  ;; (sideline-lsp-code-action ((t (:foreground ,(doom-color 'base6)))))
  :init
  (setq sideline-lsp-ignore-duplicate t
        sideline-lsp-code-actions-prefix "[!]"))

(use-package sideline
  :defer 10
  :init
  (setq sideline-backends-right '(sideline-lsp
                                  sideline-flycheck)
        sideline-backends-right-skip-current-line t
        sideline-order-right 'up
        sideline-format-right "%s"
        sideline-display-backend-name nil
        sideline-display-backend-format ""
        sideline-display-backend-type nil)
  :config
  (require 'sideline-lsp)
  (require 'sideline-flycheck)
  (require 'sideline-blame)
  (global-sideline-mode 1)
  :hook
  ((flycheck-mode-hook lsp-mode-hook prog-mode-hook blamer-mode-hook) . sideline-mode))

;; (use-package which-func
;;   :straight nil
;;   :init
;;   (setq which-func-display 'header
;;         which-func-modes '(prog-mode)
;;         which-func-update-delay 0.2
;;         which-func-format '("      "
;;                             (:propertize which-func-current
;;                                          face which-func mouse-face mode-line-highlight help-echo
;;                                          "Current function\nmouse-1: go to beginning\nmouse-2: toggle rest visibility\nmouse-3: go to end")
;;                             ""))
;;   :hook
;;   (after-init-hook . which-function-mode))

(setopt which-func-display 'header
        which-func-modes '(prog-mode)
        which-func-update-delay 0.2
        which-func-format '("      "
                            (:propertize which-func-current
                                         face which-func mouse-face mode-line-highlight help-echo
                                         "Current function\nmouse-1: go to beginning\nmouse-2: toggle rest visibility\nmouse-3: go to end")
                            ""))
(add-hook 'after-init-hook 'which-function-mode)

(provide 'init-ui)
;;; init-ui.el ends here
