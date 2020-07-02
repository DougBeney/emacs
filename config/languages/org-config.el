;;;
;; org-config.el
;; Org-Mode Configuration
;;;

(provide 'org-config)
(require 'helper-functions-config)

(setq org-root (concat (shell-command-to-string "printf $HOME") "/Sync/Org/"))
(setq org-daynotes-root (concat org-root "Day Notes/"))
(setq org-home-file (concat org-root "home.org"))

(defun org-open-root-dir ()
  (interactive)
  (dired org-root))

(defun org-open-home ()
  (interactive)
  (find-file org-home-file))

(defun org-open-file ()
  (interactive)
  (find-file
   (dougbeney/shell-cmd-output-completion
    "Find Org file: "
    (concat "find '" org-root "' -type f -iname '*.org'"))))

(defun org-open-todays-daynote ()
  (interactive)
  (let* ((todays-date (string-trim-right
                       (shell-command-to-string "date '+%F-%A'")))
         (todays-daynote-fname (concat todays-date ".org"))
         (todays-daynote (concat org-daynotes-root todays-daynote-fname)))

    (shell-command (concat "mkdir -p '" org-daynotes-root "'"))

    (when (not (file-exists-p todays-daynote))
      (shell-command (concat "echo '#+title: " todays-date " Notes\n\n* ' > '" todays-daynote "'")))

    (find-file (concat org-daynotes-root todays-daynote-fname ))))

(use-package org
  :bind (("C-c o" . org-open-file)
         ("C-c C-o" . org-open-home)
         ("C-c \\" . org-open-todays-daynote)
         ("C-c C-\\" . org-open-root-dir)))
