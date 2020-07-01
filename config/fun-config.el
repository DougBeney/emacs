;;;
;; fun-config.el
;; Configure fun, entertaining Emacs packages that are likely anti-productive.
;;;

(provide 'fun-config)

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
