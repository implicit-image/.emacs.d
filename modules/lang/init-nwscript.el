;;;; -*- lexical-binding: t -*-
;;;; Support for Bioware's Neverwinter Script language.


(use-package lsp-nwscript
  :straight (lsp-nwscript :type git
                          :host github
                          :repo "implicit-image/lsp-nwscript.el"
                          :files ("lsp-nwscript.el")))

(use-package nwscript-mode
  :autoload (nwscript-mode)
  :mode "\\.nss\\'"
  :straight (nwscript-mode :type git
                           :host github
                           :branch "master"
                           :repo "implicit-image/nwscript-mode.el")
  :init
  (defun +nwscript--setup ()
    (interactive)
    (setq-local case-fold-search t)
    (add-hook 'completion-at-point-functions (cape-capf-super (cape-company-to-capf 'company-dabbrev-code)
                                                              'cape-file))
    (funcall-interactively 'untabify (point-min) (point-max))
    (save-buffer)
    (indent-tabs-mode -1))
  :hook
  (nwscript-mode-hook . +nwscript--setup)
  :general
  (nwscript-mode-map
   :states 'normal
   :prefix "SPC"
   :non-normal-prefix "C-c SPC"
   "s o" '("Outline" . consult-outline)))

(provide 'init-nwscript)
