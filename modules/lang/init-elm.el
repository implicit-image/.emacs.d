;;; -*- lexical-binding: t -*-

(use-package elm-mode
  :init
  (setq elm-reactor-port 6969
	elm-indent-offset 4
	elm-format-on-save t))

(provide 'init-elm)
