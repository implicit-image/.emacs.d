;;; -*- lexical-binding: t -*-

(use-package lspce
  :straight `(lspce :type git
		   :host github
		   :repo "zbelial/lspce"
		   :files (:defaults
			   ,(pcase system-type
			      ('gnu/linux "lspce-module.so")
			      ('darwin "lspce-module.dylib")))
		   :pre-build ,(pcase system-type
				 ('gnu/linux '(("cargo" "build" "--release") ("cp" "./target/release/liblspce_module.so" "./lspce-module.so")))
				 ('darwin '(("cargo" "build" "--release") ("cp" "./target/release/liblspce_module.dylib" "./lspce-module.dylib"))))))


(provide 'init-lspce)
