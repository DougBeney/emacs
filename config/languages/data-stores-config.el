;;;
;; data-stores-config.el
;; This file stores configuration for files such as json or yaml.
;;;

(provide 'data-stores-config)

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package json-mode
  :mode "\\.json\\'")

(use-package conf-mode
  :ensure nil
  :mode "sxhkdrc\\'")
