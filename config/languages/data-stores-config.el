;; -*- lexical-binding: t; -*-
;;;
;; data-stores-config.el
;; This file stores configuration for files such as json or yaml.
;;;
(provide 'data-stores-config)

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" "\\.ya?ml\\.j2\\'"))

(use-package meson-mode
  :mode "meson//.build\\'")

(use-package conf-mode
  :ensure nil
  :straight nil
  :mode "\\.env\\(?:\\.[a-zA-Z0-9._-]+\\)?\\'")

(use-package caddyfile-mode)
