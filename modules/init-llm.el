;;; -*- lexical-binding: t -*-

(use-package mcp
  :straight (mcp :host github
                 :type git
                 :repo "lizqwerscott/mcp.el"))

(use-package aidermacs
  :straight (aidermacs :type git
                       :host github
                       :branch "main"
                       :repo "MatthewZMD/aidermacs")
  :config
  (aidermacs-setup-minor-mode)
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet")
  :bind*
  ( :map meow-llm-global-map
    ("A" . aidermacs-transient-menu)))

(use-package gptel
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
  (gptel-mode-hook . visual-line-mode)
  :bind*
  ( :map meow-llm-global-map
    ("g" . gptel-menu)
    ("r" . gptel-rewrite)
    :map embark-region-map
    ("R" . gptel-rewrite)))

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
