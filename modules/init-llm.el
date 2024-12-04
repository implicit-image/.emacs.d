;;; -*- lexical-binding: t -*-

(use-package gptel
  :general
  (+leader-keys
    "a i" '("Gptel menu" . gptel-menu)))

(use-package ellama)

(use-package aider
  :straight (aider
	     :host github
	     :repo "tninja/aider.el"
	     :files ("aider.el"))
  :config
  ;; Use claude-3-5-sonnet cause it is best in aider benchmark
  (setq aider-args '("--model" "anthropic/claude-3-5-sonnet-20241022")))

(use-package codeium
  :straight
  (codeium :type git
	   :host github
	   :repo "Exafunction/codeium.el"))

(use-package elysium
  :straight (elysium :type git
	     :host github
		     :repo "lanceberge/elysium"
		     :branch "main"
		     :files ("*.el"))
  :custom
  (elysium-window-size 0.33)
  (elysium-window-style 'vertical))

(provide 'init-llm)
