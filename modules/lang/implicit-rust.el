;;; -*- lexical-binding: t -*-
(require 'transient)
(require 'implicit-transient-utils)


(defun +rust--compile ()
  (compile "cargo build"))


(+transient/define-infixes!
  (+rust--cargo-color :description "Output color"
                      :argument "--color"
                      :choices '("auto" "always" "never"))
  (+rust--cargo-quiet :description "Quiet"
                      :argument "--quiet")
  (+rust--cargo-locked :description "Locked"
                       :argument "Locked")
  (+rust--cargo-offline :description "Offline"
                        :argument "--offline")
  (+rust--cargo-frozen :description "Frozen"
                       :argument "--frozen")
  (+rust--cargo-config :description "Set Config"
                       :argument "--config"))

(transient-define-suffix +rust--cargo-build ()
  "Transient prefix for cargo run."
  (interactive)
  (message "build"))

(transient-define-suffix +rust--cargo-test ()
  (interactive)
  (message "test"))

(transient-define-suffix +rust--cargo-doc ()
  (interactive)
  (message "doc"))

(transient-define-suffix +rust--cargo-run ()
  (interactive)
  (message "run"))

(transient-define-suffix +rust--cargo-init ()
  (interactive)
  (message "init"))

(transient-define-prefix +rust-command-prefix ()
  "Transient prefix for rust."
  [["Commands"
    ("b" "build" +rust--cargo-build)
    ("t" "test" +rust--cargo-test)
    ("d" "doc" +rust--cargo-doc)
    ("r" "run" +rust--cargo-run)
    ("i" "init" +rust--cargo-init)]
   ["Options"
    ("-c" "color" +rust--cargo-color)
    ("-q" "quiet" +rust--cargo-quiet)
    ("-l" "locked" +rust--cargo-locked)
    ("-o" "offline" +rust--cargo-offline)
    ("-f" "frozen" +rust--cargo-frozen)
    ("-C" "config" +rust--cargo-config)]])

(defun +rust-setup ()
  "Setup local options for rust mode buffer."
  (+set-buffer-functions! ))

(provide 'implicit-rust)
