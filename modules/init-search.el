;;; -*- lexical-binding: t -*-

(use-package grep
  :straight nil
  :init
  (setq find-program (+os/per-system! :win (shell-quote-argument "c:/Program Files/Git/usr/bin/find.exe")
                                      :linux "find"
                                      :wsl "find"))

  (setq grep-program (or (executable-find "rg")
                         (executable-find "grep"))
        grep-find-ignored-directories '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist")
        )
  ;; for preview in grep mode
  (defun +next-error-no-select (&optional n)
    (interactive "p")
    (save-selected-window
      (let ((next-error-highlight next-error-highlight-no-select)
            (display-buffer-overriding-action
             '((display-buffer-reuse-window display-buffer-reuse-mode-window)
               (inhibit-same-window . t))))
        (next-error n))))

  (defun +previous-error-no-select (&optional n)
    (interactive "p")
    (+next-error-no-select (- (or n 1)))))

;; :general
;; (grep-mode-map
;;  :states 'normal
;;  "M-n" '+next-error-no-select
;;  "M-p" '+previous-error-no-select))

(use-package deadgrep
  :custom
  (deadgrep-display-buffer-function '+deadgrep-display-buffer-function)
  :init
  (defun +deadgrep-display-buffer-function (buffer)
    (display-buffer-same-window buffer nil)))
;; :general
;; (deadgrep-edit-mode-map
;;  :states 'normal
;;  "Z Z" 'deadgrep-mode)
;; (rg-global-map
;;  "?" 'deadgrep))

(use-package rg
  :init
  (setq rg-align-position-numbers t))
;; :general
;; (general-override-mode-map
;;  :states '(normal visual)
;;  "g /" '(:which-key "rg" :keymap rg-global-map :package rg))
;; (rg-global-map
;;  "d" 'ignore
;;  "/" 'rg-dwim
;;  "p" 'rg-project
;;  "s" 'rg-isearch-project
;;  "S" 'rg-isearch-current-dir
;;  "f" 'rg-isearch-current-file))

(defun +search/rg-thing-at-point ()
  (interactive)
  (let ((s (symbol-at-point)))
    (consult-ripgrep
     (cond ((bound-and-true-p projectile-project-root) (projectile-project-root))
           ((bound-and-true-p project-root) (project-root))
           (t default-directory))
     (if (eq s nil) "" (symbol-name s)))))

(use-package imenu
  :straight nil
  :config
  (setq imenu-auto-rescan t
        imenu-use-popup-menu 'on-mouse))

(use-package isearch
  :straight nil)

;; (+leader-keys
;;   "*" '("Find thing-at-point in project" . +search/rg-thing-at-point))

(provide 'init-search)
