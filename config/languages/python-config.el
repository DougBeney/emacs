;;;
;; python-config.el
;;;

(provide 'python-config)

(use-package python
  :hook (python-mode . lsp)
  :config
  (setenv "PYTHONUSERBASE" "/home/doug/.local/opt/packages/python")
  (setq python-shell-interpreter "python3"))
