;;; -*- lexical-binding: t -*-

(use-package elysium
  :straight (elysium :type git
                     :host github
                     :repo "lanceberge/elysium"
                     :branch "main"
                     :files ("*.el"))
  :custom
  (elysium-window-size 0.33)
  (elysium-window-style 'vertical))


(provide 'init-elysium)
