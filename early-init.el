;;; early-init.el early-init emacs file -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Błażej Niewiadomski
;; Author: Błażej Niewiadomski <blaz.nie@protonmail.com>
;; This file is not part of GNU Emacs.
;;; Commentary:
;;
;;
;;; Code:

;; to increase lsp-mode performance
(setenv "LSP_USE_PLISTS" "true")

;; Don't flicker GUI elements on startup
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; We're using straight.el instead of package.el, no need to load it
(setq package-enable-at-startup nil
      evil-want-keybinding nil)


(when (member "--light" command-line-args)
  (message "light detected")
  (setq load-light-init t)
  (setq command-line-args (delete "--light" command-line-args)))

;;; early-init.el ends here
