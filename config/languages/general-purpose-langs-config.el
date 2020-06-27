;;;
;; general-purpose-langs-config.el
;; General purpose languages like Python, Ruby, or even C++
;;;

(provide 'general-purpose-langs-config)

(use-package elpy
  :init
  (elpy-enable)
  (setq elpy-rpc-virtualenv-path 'default)
  (setq elpy-rpc-python-command "python3"))

(use-package ruby-mode
  :hook (ruby-mode . lsp))
