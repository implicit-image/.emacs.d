;;; -*- lexical-binding: t -*-

(use-package init-nand2tetris
  :commands
  (jack-mode hdl-mode tst-mode vm-mode)
  :straight (init-nand2tetris :type git
			 :host github
			 :repo "Deng-Li3/emacs-nand2tetris"))

(provide 'init-hdl)
