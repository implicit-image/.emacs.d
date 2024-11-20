(use-package nand2tetris
  :mode "\\.hdl\\'"
  :straight (nand2tetris :type git
			 :host github
			 :repo "CestDiego/nand2tetris.el")
  :config
  (setq nand2tetris-core-base-dir "~/projects/nand2tetris/core"))

(provide 'init-hdl)
