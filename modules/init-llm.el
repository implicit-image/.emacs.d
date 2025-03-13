;;; -*- lexical-binding: t -*-


(use-package gptel
  :init
  (defun +gptel/get-host-password (host &optional num)
    (let (num ())))
  :config
  (setq gptel-model 'DeepSeek-R1
        +gptel-azure-dsr1 (gptel-make-azure "Azure-1"
                            :protocol "https"
                            :host "DeepSeek-R1-nebbc.eastus2.models.ai.azure.com"
                            :endpoint "/v1/chat/completions?api-version=2023-05-15"
                            :stream t
                            :header `(("Authorization" . ,(gptel-api-key-from-auth-source)))
                            :key #'gptel-api-key
                            :models '(DeepSeek-R1))
        +gptel-claude (gptel-make-anthropic "Claude"
                        :stream t
                        :key (plist-get)))
  :hook
  (gptel-mode . visual-line-mode)
  :general
  (+leader-keys
    "a i" '("Gptel menu" . gptel-menu)))


(use-package ellama)

(use-package aider
  :straight (aider
             :host github
             :repo "tninja/aider.el"
             :files ("aider.el"))
  :init
  (+windows-cfg '(("*aider")
                  :position right :width 0.5 :regexp t))
  :config
  (require 'init-aider)

  (setq aider-args `("--model" "azure/DeepSeek-R1-nebbc.eastus2.models.ai.azure.com"))
  :general
  (+leader-keys
    "a a" '("[AI]der menu" . aider-transient-menu)))

(use-package aidermacs
  :config
  (aidermacs-setup-minor-mode)
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet")
  :general
  (+leader-keys
    "a A" '("Aidermacs menu." . aidermacs-transient-menu)))

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
