;;;
;; global-keybindings-config.el
;;;

(provide 'global-keybindings-config)

(require 'helper-functions-config)
(require 'config-editing-config)

;; Remapping save-buffers-kill-emacs
;; It's a handy function but I just accidentally hit it a lot
(global-unset-key (kbd "C-x C-c")) ; Remapping save-buffers-kill-emacs
(global-unset-key (kbd "C-z"))     ; Remapping suspend-frame

(global-set-key (kbd "M-s") #'isearch-forward)
(global-set-key (kbd "M-r") #'isearch-backward)

(global-set-key (kbd "M-;") #'comment-dwim)

(global-set-key (kbd "C-c r") #'reload-config)

;; Useful in C/C++ projects. Switch between the .c and .h file
(global-set-key (kbd "C-c 0") 'ff-find-other-file)

;; Open dired in current directory
(global-set-key (kbd "C-c d") (lambda () (interactive) (dired ".")))

;; Change-directory related bindings
(global-set-key (kbd "C-c p") (lambda () (interactive) (dired "~/Code")))

;; Undo tree
(global-set-key (kbd "C-z") #'undo-only)
(global-set-key (kbd "C-S-z") #'undo-redo)

;; Dupliate a line
(global-set-key (kbd "C-c C-d") (kbd "C-a C-SPC C-n M-w C-y C-p"))
