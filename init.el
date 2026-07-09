(load "~/.emacs.d/sanemacs-elpaca.el" nil t) ; https://sanemacs.com

;; Uncomment when debugging errors
;; (toggle-debug-on-error)

;; Time to load up my config!
;; All of my main configuration files are in ~/.emacs.d/config/
;; Programming language-specific configurations are in ~/.emacs.d/config/languages/

(add-to-list 'load-path "~/.emacs.d/config/")
(add-to-list 'load-path "~/.emacs.d/config/languages/")

;; Little helper function to make loading all my configs easier.
(defun load-config (&rest args)
  (dolist (arg args)
    (require (intern (format "%s-config" arg)))))

(load-config 'theming
             'typography
             'global-keybindings
             'env
             'helper-functions
             'indentation-and-whitespace
             'data-stores
             'general-purpose-langs
             'workflow
             'shell-langs
             'web
             'writing
             'org
             'fun
             'config-editing
             'ai
             'email
             'space-leader-map
             )

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;(setq byte-compile-warnings '(cl-functions)) ; Supress cl deprecated warning
