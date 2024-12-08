;;; -*- lexical-binding: t -*-

(use-package auth-source
  :demand
  :config
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")))

(use-package tramp)


(provide 'init-remote)
