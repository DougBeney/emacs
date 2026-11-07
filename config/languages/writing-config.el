;; -*- lexical-binding: t; -*-

;;;
;; writing-config.el
;; Filetypes for writing, such as markdown. (go to org-config.el for org-mode configuration)
;;;

(provide 'writing-config)

(use-package markdown-mode
   :mode "//.md\\'"
   :mode "\\.markdown\\'"
   :custom
   (markdown-hide-markup t)
   (markdown-header-scaling t))

(use-package define-word)

;; (defun dougbeney/olivetti-font ()
;;   (setq buffer-face-mode-face '(:family "EB Garamond" :height 150))
;;   (setq line-spacing 1)
;;   (buffer-face-mode))

(use-package olivetti
  :hook (markdown-mode . olivetti-mode)
  :init
  (setq-default olivetti-body-width 108)
  (setq-default olivetti-style 'fancy))

(setq-default truncate-lines nil)
