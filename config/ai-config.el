(provide 'ai-config)

(use-package agent-shell)

(use-package gptel
    :config
  (setq gptel-include-reasoning nil
        gptel-system-prompt
        "You are a helpful assistant. You respond in Emacs Org-mode markup. always use headings that are deeper than the current heading level. Never use the same level or shallower headings. Any quotations or code blocks should be surrounded my proper org-mode markup. Follow org-mode best practices. Never answer your message with follow-up questions to the user."

        gptel-directives
        `(
         (default . ,gptel-system-prompt)
         (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note."))

        gptel-org-convert-response nil ; Disable the totally unnecessary markdown->org conversion function
        gptel-default-mode 'org-mode   ; Major mode for dedicated chat buffers

        gptel-prompt-prefix-alist
        '((markdown-mode . "# ") (org-mode . "* ") (text-mode . "$ "))
        )
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

(use-package mcp
    :ensure t
    :after gptel
    :init
    (defcustom mcp--searxng-url nil
      "URL of searx-ng instance"
      :type 'string
      :group 'mcp)
      :custom (mcp-hub-servers
              `(("searxng" . (:command "npx"
                              :args ("-y" "mcp-searxng")
                              :env (:SEARXNG_URL mcp--searxng-url)))))
     :config (require 'mcp-hub)
                 :hook (after-init . mcp-hub-start-all-server))
