;;; -*- lexical-binding: t -*-

(use-package elm-mode
  :preface (add-to-list '+lsp/servers-to-install 'elm-ls)
  :init
  (setq elm-reactor-port 6969
	elm-indent-offset 4
	elm-format-on-save t))

(provide 'init-elm)
