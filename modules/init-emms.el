;;; -*- lexical-binding: t -*-

(use-package emms
  :init
  (setq emms-player-list '()
	emms-source-file-default-directory (getenv "XDG_MUSIC_DIR")))

(provide 'init-emms)
