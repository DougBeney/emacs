;;;
;; fun-config.el
;; Configure fun, entertaining Emacs packages that are likely anti-productive.
;;;

(provide 'fun-config)

(use-package eww
  :ensure nil
  :init
  (setq eww-search-prefix "https://lite.duckduckgo.com/lite/?q="))

(use-package md4rd
  :commands md4rd
  :bind (("C-c C-r" . md4rd))
  :config
  (setq md4rd-subs-active '(lisp+Common_Lisp+racket
                            emacs
                            linux
                            kde+gnome
                            bigseo
                            opensource+freesoftware
                            podcasts
                            privacy+privacytoolsIO
                            programming
                            webdev
                            python
                            usabilityporn
                            nosurf
                            autodetailing
                            jeepzj
                            entrepreneur+startups+hwstartups)))

(use-package csv-mode
  :mode "\\.csv\\'"
  :hook (csv-mode . csv-align-mode))

(use-package openwith
  :config
  (setq openwith-associations '(("\\.xlsx\\'" "libreoffice" (file))
                                ("\\.\\(jpe?g\\|png\\)" "eog" (file))))
  (openwith-mode t))

;; (use-package edit-server
;;   :commands edit-server-start
;;   :init
;;   (if after-init-time
;;       (edit-server-start)
;;     (add-hook 'after-init-hook
;;               #'(lambda() (edit-server-start))))
;;   :config
;;   (setq edit-server-new-frame-alist
;;         '((name . "Edit with Emacs FRAME")
;;           (top . 200)
;;           (left . 200)
;;           (width . 80)
;;           (height . 25)
;;           (minibuffer . t)
;;           (menu-bar-lines . t)
;;           (window-system . x))))

(use-package activity-watch-mode
  :init
  (global-activity-watch-mode))

(use-package lorem-ipsum)
