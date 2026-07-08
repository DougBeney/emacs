;;;
;; data-stores-config.el
;; This file stores configuration for files such as json or yaml.
;;;

(provide 'data-stores-config)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; (use-package json-mode
;;   :mode "\\.json\\'")

(use-package meson-mode
  :mode "meson//.build\\'")

(use-package conf-mode
  :ensure nil
  :straight nil
  :mode "\\.env\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'")

(use-package caddyfile-mode)
