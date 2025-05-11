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
  (gptel-mode-hook . visual-line-mode))
;; :general
;; (+leader-keys
;;   "a i" '("Gptel menu" . gptel-menu)))





(provide 'init-gptel)
