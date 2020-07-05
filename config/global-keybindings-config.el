;;;
;; global-keybindings-config.el
;;;

(provide 'global-keybindings-config)

(require 'helper-functions-config)
(require 'config-editing-config)

(global-set-key (kbd "C-c r") #'reload-config)

(global-set-key (kbd "C-c C-e") #'dougbeney/edit-emacs-config)
(global-set-key (kbd "C-c e") #'dougbeney/edit-emacs-config-file)

(global-set-key (kbd "C-c C-x b") #'dougbeney/edit-bspwm-config-file)
(global-set-key (kbd "C-c C-x C-b") #'dougbeney/bspwm-dir)

;; Useful in C/C++ projects. Switch between the .c and .h file
(global-set-key (kbd "C-c 0") 'ff-find-other-file)

;; Duplicate line
(global-set-key (kbd "C-c C-d") (kbd "C-a C-SPC C-n M-w C-y C-p C-a"))

;; Open dired in current directory
(global-set-key (kbd "C-c d") (lambda () (interactive) (dired ".")))

;; Change-directory related bindings
(global-set-key (kbd "C-c p") (lambda () (interactive) (dired "~/Code")))
(global-set-key (kbd "C-c c w") (lambda () (interactive) (dired "~/Google Drive/Work/Client Work")))
(global-set-key (kbd "C-c b") #'dougbeney/view-blog-posts)

;; Terminal related bindings
(global-set-key (kbd "M-7") 'dougbeney/terminal)
(global-set-key (kbd "C-c t") #'dougbeney/open-terminal-in-workdir)
