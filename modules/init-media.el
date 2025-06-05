;;; -*- lexical-binding: t -*-

(setq max-image-size 15.0
      image-auto-resize t)

(use-package emms
  :init
  (setq emms-player-list '(emms-player-mpd)
        emms-source-file-default-directory (getenv "XDG_MUSIC_DIR"))
  :config
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (setq emms-player-mpd-server-name "localhost"
        emms-player-mpd-server-port "6600"
        emms-player-mpd-music-directory (substitute-in-file-name "$XDG_MUSIC_DIR"))
  (emms-player-mpd-connect)
  :hook
  (emms-info-functions . emms-info-mpd))

(provide 'init-media)
