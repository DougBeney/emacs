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
  :mode "\\.json\\'"
  :mode "\\.php\\'"
  :config
  (setq web-mode-enable-auto-pairing nil)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
  (web-mode-use-tabs)
  (emmet-mode 1))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package scss-mode
  :mode "\\.scss\\'")
