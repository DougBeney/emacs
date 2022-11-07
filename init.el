(load "~/.emacs.d/sanemacs.el" nil t) ; https://Sanemacs.com
(setq byte-compile-warnings '(cl-functions))

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
             'package-management
             'env
             'commands
             'helper-functions
             'indentation-and-whitespace
             'workflow
             'data-stores
             'general-purpose-langs
             'shell-langs
             'web
             'writing
             'org
             'fun
             'config-editing)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq byte-compile-warnings '(cl-functions)) ; Supress cl deprecated warning
