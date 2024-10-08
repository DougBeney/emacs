;;;
;; general-purpose-langs-config.el
;; General purpose languages like Python, Ruby, or even C++
;;;

;; (global-font-lock-mode -1)

;; (use-package tree-sitter
;;   :config
;;   (global-tree-sitter-mode)
;;   (use-package tree-sitter-langs))

(provide 'general-purpose-langs-config)

;; (use-package elpy
;;   :init
;;   ;; (setenv "PYTHONUSERBASE" "/home/doug/.local/opt/packages/python")
;;   ;; (setq python-shell-interpreter "python3")
;;   ;; (setq elpy-rpc-virtualenv-path 'default)
;;   ;; (setq elpy-rpc-python-command "python3")
;;   (setq elpy-syntax-check-command "flake8 --ignore W191,E501")
;;   (elpy-enable))

(use-package ruby-mode
  ;; :hook (ruby-mode . lsp)
  )

(use-package lsp-mode
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
		 (python-mode . lsp)
		 (css-mode . lsp)
		 (scss-mode . lsp))
  :init
  (setq gc-cons-threshold 100000000)
  (setq lsp-idle-delay 0.500)
  (setq lsp-keymap-prefix "C-c l")
  :config
  (use-package lsp-ui :commands lsp-ui-mode))

(use-package flycheck)

;; (use-package lsp-pyright
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp))))

;(use-package eglot)
(use-package pyvenv)
