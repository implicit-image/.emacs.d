;;; -*- lexical-binding: t -*-

(setq find-program (+os/per-system! :win (shell-quote-argument "c:/Program Files/Git/usr/bin/find.exe")
                                    :linux "find"
                                    :wsl "find")
      grep-program (or (executable-find "rg")
                       (executable-find "grep"))
      grep-find-ignored-directories '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "build" "dist")
      isearch-wrap-pause 'no
      multi-isearch-pause t)

;; (use-package deadgrep
;;   :custom
;;   (deadgrep-display-buffer-function '+deadgrep-display-buffer-function)
;;   :bind*
;;   ( :map meow-grep-global-map
;;     ("?" . deadgrep)))

(use-package rg
  :init
  (setq rg-align-position-numbers t)
  :bind*
  (("M-s /" . rg-isearch-project)
   :map meow-grep-global-map
   ("RET" . rg-dwim)
   ("p" . rg-project)
   ("s" . rg-isearch-project)
   ("S" . rg-isearch-current-dir)
   :map rg-mode-map
   ("i" . wgrep-change-to-wgrep-mode)))

(use-package wgrep
  :bind*
  ( :map grep-mode-map
    ("i" . wgrep-change-to-wgrep-mode)))

(setopt imenu-auto-rescan t
        imenu-use-popup-menu 'on-mouse)

(bind-keys*
 ("M-g /" . +search/rg-thing-at-point)
 :map meow-grep-global-map
 ("*" . +search/rg-thing-at-point))

(provide 'init-search)
