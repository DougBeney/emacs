;;;
;; writing-config.el
;; Filetypes for writing, such as markdown. (go to org-config.el for org-mode configuration)
;;;

(provide 'writing-config)

(use-package markdown-mode
   :mode "//.md\\'"
   :mode "\\.markdown\\'"
   ;; :hook (markdown-mode . (lambda ()
   ;;                          (dougbeney/set-font-local dougbeney-term-mode-face
   ;;                                                    (:family "Liberation Serif"
   ;;                                                             :height 150))
   ;;                          (fringe-mode -1)))
   :init
   ;; (setq markdown-header-scaling t)
    )

(use-package define-word)

;; (defun dougbeney/olivetti-font ()
;;   (setq buffer-face-mode-face '(:family "EB Garamond" :height 150))
;;   (setq line-spacing 1)
;;   (buffer-face-mode))

(use-package olivetti
  :init
  ;(setq-default olivetti-body-width nil)
  ;; (setq-default olivetti-style 'fancy)
  ;; (add-hook 'org-mode-hook #'olivetti-mode)
  )

(setq-default truncate-lines nil)
