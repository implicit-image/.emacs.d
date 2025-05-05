;;; init-ada.el -*- lexical-binding: t -*-

;;;; Editing Ada source files.
(use-package ada-mode
  :init
  (setq ada-indent-use 4
        ada-indent-when 4))


;;;; Editing GNAT files.
(use-package gpr-mode)

(provide 'init-ada)
