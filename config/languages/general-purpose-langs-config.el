;;;
;; general-purpose-langs-config.el
;; General purpose languages like Python, Ruby, or even C++
;;;

(provide 'general-purpose-langs-config)

;; (use-package elpy
;;   :init
;;   (setenv "PYTHONUSERBASE" "/home/doug/.local/opt/packages/python")
;;   (setq python-shell-interpreter "python3")
;;   (setq elpy-rpc-virtualenv-path 'default)
;;   (setq elpy-rpc-python-command "python3")
;;   (elpy-enable))

(use-package ruby-mode
  :hook (ruby-mode . lsp))

(use-package lsp-mode
  :hook ((c-mode-hook . lsp)
         (c++-mode-hook . lsp))
  :commands lsp
  :init
  (setq gc-cons-threshold 100000000)
  (setq lsp-idle-delay 0.500)
  (setq lsp-keymap-prefix "C-c l")
  :config
  (use-package lsp-ui :commands lsp-ui-mode)
  (use-package lsp-ivy :commands lsp-ivy-workspace-symbol))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))
