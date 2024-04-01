;;;
;; writing-config.el
;; Filetypes for writing, such as markdown. (go to org-config.el for org-mode configuration)
;;;

(provide 'writing-config)

(use-package markdown-mode
  :mode "//.md\\'"
  :mode "\\.markdown\\'"
  :hook (markdown-mode . (lambda ()
                           (olivetti-mode 1)
                           (dougbeney/set-font-local dougbeney-term-mode-face
                                                     (:family "Liberation Serif"
                                                              :height 150))
                           (fringe-mode -1)))
  :init
  (setq markdown-header-scaling t))

(use-package define-word)

(use-package olivetti
  :init
  (setq-default olivetti-body-width 140))
