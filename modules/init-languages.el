;;; -*- lexical-binding: t -*-
(require 'f)

(require 'init-c)
(require 'init-web)
(require 'init-python)

;;;; jvm supported languages
(require 'init-jvm)
(require 'init-scala)
(require 'init-java)
(require 'init-kotlin)
(require 'init-groovy)
(require 'init-clojure)

(require 'init-haskell)
(require 'init-rust)
(require 'init-lisp)
(require 'init-dart)
(require 'init-purescript)
(require 'init-R)
(require 'init-gleam)
(require 'init-erlang)
(require 'init-elixir)
(require 'init-go)
(require 'init-ruby)
(require 'init-dotnet)
(require 'init-zig)
(require 'init-asm)
(require 'init-idris)
(require 'init-ada)
(require 'init-elm)
(require 'init-lua)
(require 'init-nim)
(require 'init-gdscript)
(require 'init-d)
(require 'init-lean)
(require 'init-sql)
(require 'init-hdl)
(require 'init-solidity)
(require 'init-plantuml)

;;;; languages for nand2tetris course
(use-package init-nand2tetris
  :commands
  (jack-mode hdl-mode tst-mode vm-mode)
  :straight (init-nand2tetris :type git
                              :host github
                              :repo "Deng-Li3/emacs-nand2tetris"))

;;;; ml family languages
(require 'init-ml)

;;;; LaTeX editing
(require 'init-latex)

;;;; various shell languages
(require 'init-shell)

;;;; common configuration languages
(require 'init-conf)

;;;; cuda support
(require 'init-cuda)

;;;; Bioware's Neverwinter Script support
(require 'init-nwscript)



(provide 'init-languages)
