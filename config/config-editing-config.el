;;;
;; config-editing-config.el
;;;

(provide 'config-editing-config)
(require 'helper-functions-config)

(defun dougbeney/edit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun dougbeney/edit-emacs-config-file ()
  (interactive)
  (let ((config_file
		 (dougbeney/shell-cmd-output-completion
		  "Select Config you wish to edit: "
		  "find ~/.emacs.d/ -maxdepth 1 -type f -iname '*.el' && find ~/.emacs.d/config -type f -iname '*.el'")))
	(find-file config_file)))

(defun dougbeney/edit-hyprland-config ()
  (interactive)
  (find-file "~/.config/hypr/hyprland.conf"))

;; Keybindings

(global-set-key (kbd "C-c C-e") #'dougbeney/edit-emacs-config)
(global-set-key (kbd "C-c C-c C-e") #'dougbeney/edit-hyprland-config)
(global-set-key (kbd "C-c e") #'dougbeney/edit-emacs-config-file)
