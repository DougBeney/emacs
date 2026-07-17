(provide 'ai-config)

(use-package agent-shell)

(use-package gptel
    :config
    (defgroup doug-gptel nil
      "Personal gptel backend settings."
      :group 'gptel)

    (defcustom doug/gptel-backend-name "Your Backend"
      "Display name of the gptel backend."
      :type 'string
      :group 'doug-gptel)

    (defcustom doug/gptel-host "litellm.example.com"
      "Hostname of the OpenAI-compatible API (no protocol prefix)."
      :type 'string
      :group 'doug-gptel)

    (defcustom doug/gptel-models '(claude-haiku-4-5)
      "Models available on the backend."
      :type '(repeat symbol)
      :group 'doug-gptel)

    (defcustom doug/gptel-default-model 'claude-haiku-4-5
      "Default model for gptel to use."
      :type 'symbol
      :group 'doug-gptel)

    (setq
     gptel-include-reasoning nil
     gptel-system-prompt
     "You are a helpful assistant. You can either respond in Emacs org-mode markup or a simple unformatted text paragraph depending on the complexity of what the user is asking and what is most appropriate for an answer.  When using org-mode heading,always use headings that are deeper than the current heading level. Never use the same level or shallower headings. Any quotations or code blocks should be surrounded my proper org-mode markup. Follow org-mode best practices. Never answer your message with follow-up questions to the user."

     gptel-directives
     `(
       (default . ,gptel-system-prompt)
       (org-paragraph . "Respond with a concise, friendly, easy to understand plain-text paragraph surround by an org-mode example block - #+BEGIN_EXAMPLE as the first line and #+END_EXAMPLE as the last line.")
       (programming . "You are a large language model and a careful programmer. Provide code and only code as output without any additional text, prompt or note."))

     gptel-org-convert-response nil ; Disable the totally unnecessary markdown->org conversion function
     gptel-default-mode 'org-mode   ; Major mode for dedicated chat buffers

     gptel-prompt-prefix-alist
     '((markdown-mode . "# ") (org-mode . "* ") (text-mode . "$ "))

     gptel-confirm-tool-calls t
     )

    (setq gptel-backend
          (gptel-make-openai doug/gptel-backend-name
            :protocol "https"
            :host doug/gptel-host
            :endpoint "/v1/chat/completions"
            :key #'gptel-api-key-from-auth-source
            :models doug/gptel-models
            :stream t)
          gptel-model 'doug/gptel-default-model))

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
