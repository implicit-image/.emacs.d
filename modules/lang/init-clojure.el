(use-package clojure-mode
  :preface
  (defun +jvm/set-up-clojure-environment ()
    "Set uo clojure environment depending on a runtime.")
  :hook
  (clojure-mode . +jvm/set-up-clojure-environment))

(use-package cider)

(use-package clojure-snippets)

(use-package clj-refactor)

(provide 'init-clojure)
