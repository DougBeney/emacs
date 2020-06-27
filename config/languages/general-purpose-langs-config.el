;;;
;; general-purpose-langs-config.el
;; General purpose languages like Python, Ruby, or even C++
;;;

(provide 'general-purpose-langs-config)

(use-package python
  :hook (python-mode . lsp)
  :mode "\\.py\\'"
  :config
  (setenv "PYTHONUSERBASE" "/home/doug/.local/opt/packages/python")
  (setq python-shell-interpreter "python3"))

(use-package elpy
  :init
  :mode "\\.py\\'"
  :config
  (elpy-enable)
  (setq elpy-rpc-virtualenv-path 'default)
  (setq elpy-rpc-python-command "python3"))

(use-package ruby-mode
  :hook (ruby-mode . lsp))
