;;;
;; general-purpose-langs-config.el
;; General purpose languages like Python, Ruby, or even C++
;;;

(provide 'general-purpose-langs-config)

(use-package elpy
  :init
  (setenv "PYTHONUSERBASE" "/home/doug/.local/opt/packages/python")
  (setq python-shell-interpreter "python3")
  (setq elpy-rpc-virtualenv-path 'default)
  (setq elpy-rpc-python-command "python3")
  (elpy-enable))

(use-package ruby-mode
  :hook (ruby-mode . lsp))
