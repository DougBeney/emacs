;;;
;; web-config.el
;; Various editing modes for html, js, css and other web-related files.
;;;

(provide 'web-config)

(use-package emmet-mode
  :hook (web-mode . emmet-mode))

(use-package web-mode
  :mode "\\.html\\'"
  :mode "\\.liquid\\'"
  :mode "\\.vue\\'"
  :mode "\\.jsx\\'"
  :mode "\\.js\\'"
  :mode "\\.php\\'"
  :config
  (web-mode-use-tabs)
  (emmet-mode 1))

;; (use-package vue-mode
;;   :mode "\\.vue\\'")

(use-package pug-mode
  :init
  (setq pug-tab-width 4)
  :mode "\\.pug\\'")

(use-package scss-mode
  :mode "\\.scss\\'")
