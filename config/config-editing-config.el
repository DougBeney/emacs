;;;
;; config-editing-config.el
;;;

(provide 'config-editing-config)
(require 'helper-functions-config)

(use-package conf-mode
  :ensure nil
  :mode "sxhkdrc\\'"
  :mode "polybar\\'")

(defun dougbeney/edit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun dougbeney/edit-emacs-config-file ()
  (interactive)
  (helm-find-files
   (dougbeney/shell-cmd-output-completion
    "Select Config you wish to edit: "
    "find ~/.emacs.d/ -maxdepth 1 -type f -iname '*.el' && find ~/.emacs.d/config -type f -iname '*.el'")))

(defun dougbeney/bspwm-dir ()
  (interactive)
  (dired "~/.config/bspwm/"))

(defun dougbeney/edit-bspwm-config-file ()
  (interactive)
  (find-file
   (dougbeney/shell-cmd-output-completion
    "Select Config you wish to edit: "
    "find ~/.config/bspwm/ -type f -not -path '*.config/bspwm/.git/*' -not -path '*/*.png'")))
