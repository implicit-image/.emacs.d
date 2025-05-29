;;; -*- lexical-binding: t -*-

(defun +search/rg-thing-at-point ()
  (interactive)
  (let ((s (symbol-at-point)))
    (consult-ripgrep
     (cond ((bound-and-true-p projectile-project-root) (projectile-project-root))
           ((bound-and-true-p project-root) (project-root))
           (t default-directory))
     (if (eq s nil) "" (symbol-name s)))))

(defun +next-error-no-select (&optional n)
  (interactive "p")
  (save-selected-window
    (let ((next-error-highlight next-error-highlight-no-select)
          (display-buffer-overriding-action
           '((display-buffer-use-least-recent-window)
             (inhibit-same-window . t))))
      (next-error n))))

(defun +previous-error-no-select (&optional n)
  (interactive "p")
  (+next-error-no-select (- (or n 1))))

(setq find-program (+os/per-system! :win (shell-quote-argument "c:/Program Files/Git/usr/bin/find.exe")
                                    :linux "find"
                                    :wsl "find"))

(setq grep-program (or (executable-find "rg")
                       (executable-find "grep"))
      grep-find-ignored-directories '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist"))

;; (use-package grep
;;   :straight nil
;;   :init

;; for preview in grep mode
;; :bind*
;; ( :map grep-mode-map
;;   ("M-n" . +next-error-no-select)
;;   ("M-p" . +previous-error-no-select)))

(use-package deadgrep
  :custom
  (deadgrep-display-buffer-function '+deadgrep-display-buffer-function)
  :init
  (defun +deadgrep-display-buffer-function (buffer)
    (display-buffer-same-window buffer nil)))
;; :bind*
;; ( :map rg-global-map
;;   ("?" . deadgrep)))

(use-package rg
  :init
  (setq rg-align-position-numbers t))
;; :bind*
;; ( :map rg-global-map
;;   ("d" . igneeee("h" . help-map)
;;   ("/" . rg-dwim)
;;   ("p" . rg-project)
;;   ("s" . rg-isearch-project)
;;   ("S" . rg-isearch-current-dir)
;;   ("f" . rg-isearch-current-file)))

;; (use-package imenu
;;   :straight nil
;;   :config
;;   (setq imenu-auto-rescan t
;;         imenu-use-popup-menu 'on-mouse))

(setopt imenu-auto-rescan t
        imenu-use-popup-menu 'on-mouse)

;; (use-package isearch
;;   :straight nil)

;; (bind-key "C-x <space> *" '+search/rg-thing-at-point)

(provide 'init-search)
