;;; -*- lexical-binding: t -*-


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
  :config

  (setq aider-args `("--model" "azure/DeepSeek-R1-nebbc.eastus2.models.ai.azure.com"))
  :general
  (+leader-keys
    "a a" '("[AI]der menu" . aider-transient-menu)))

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
  :general
  (+leader-keys
    "a A" '("Aidermacs menu." . aidermacs-transient-menu)))

(provide 'init-aider)
