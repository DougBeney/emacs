(provide 'ai-config)

(use-package agent-shell)

(use-package gptel
  :config
  (setq gptel-include-reasoning nil)
  ;; Configure the below securely
  ;; (setq
  ;;  gptel-model 'openai/example-model
  ;;  gptel-backend (gptel-make-openai "My OpenAI compatible API"
  ;;                  :host "host.example-llm.com"
  ;;                  :endpoint "/v1/chat/completions"
  ;;                  :stream t
  ;;                  :protocol "https"
  ;;                  :key "secret-key-here" ;can be a function that returns the key
  ;;                  :models '(openai/example-model)))
  )
