
(use-package mcp
  :straight (mcp :host github
                 :type git
                 :repo "lizqwerscott/mcp.el"))

(use-package aider
  :straight (aider
             :host github
             :repo "tninja/aider.el"
             :files ("aider.el"))
  :init
  (defun +aider/setup ()
    "Setup aider environment."
    (let* ((auth-info (car (auth-source-search :host "DeepSeek-R1-nebbc.eastus2.models.ai.azure.com"
                                               :max 1)))
           (key (plist-get auth-info :secret))
           (key-value (pcase (type-of key)
                        ((or 'byte-code-function
                             'function)
                         (funcall key))
                        ('string key)
                        (_ nil)))
           (api-base (plist-get auth-info :host))
           (api-version "2023-05-15"))
      (when key-value
        (exec-path-from-shell-setenv "AZURE_API_KEY" key-value))
      (when api-base
        (exec-path-from-shell-setenv "AZURE_API_BASE" api-base))
      (when api-version
        (exec-path-from-shell-setenv "AZURE_API_VERSION" api-version))))

  (setq aider-args `("--model" "azure/DeepSeek-R1-nebbc.eastus2.models.ai.azure.com")))

(use-package aidermacs
  :straight (aidermacs :type git
                       :host github
                       :branch "main"
                       :repo "MatthewZMD/aidermacs")
  :config
  (aidermacs-setup-minor-mode)
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet"))

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
  (gptel-mode-hook . visual-line-mode))

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
