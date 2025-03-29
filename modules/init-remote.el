;;; -*- lexical-binding: t -*-

(use-package auth-source
  :demand
  :config
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" "~/.netrc")))

(use-package tramp
  :config
  ;; change sudo timeout
  (add-to-list 'tramp-connection-properties '(nil
                                              "session-timeout"
                                              "240")))

(provide 'init-remote)
