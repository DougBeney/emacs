;;;
;; web-config.el
;; Various editing modes for html, js, css and other web-related files.
;;;

(provide 'web-config)

(use-package emmet-mode
  :hook (web-mode . emmet-mode))

(use-package web-mode
  :mode "\\.html?\\'"
  :mode "\\.vue\\'"
  :mode "\\.js\\'"
  :mode "\\.php\\'")

;; (use-package js
;;   :mode "\\.js\\'"
;;   :hook (js-mode . lsp))

(use-package pug-mode
  :init
  (setq pug-tab-width 4)
  :mode "\\.pug\\'")

(use-package sass-mode
  :mode "\\.scss\\'"
  :mode "\\.sass\\'")
