;;;
;; org-config.el
;; Org-Mode Configuration
;;;

(provide 'org-config)
(require 'helper-functions-config)

(setq org-root (concat (shell-command-to-string "printf $HOME") "/Sync/Org/"))
(setq org-daynotes-root (concat org-root "Day Notes/"))
(setq org-index-file (concat org-root "index.org"))

(defun org-open-index ()
  (interactive)
  (find-file org-index-file))

(defun org-open-file ()
  (interactive)
  (find-file
   (dougbeney/shell-cmd-output-completion
    "Find Org file: "
    (concat "find " org-root " -type f -iname '*.org'"))))

(defun org-open-todays-daynote ()
  (interactive)
  (shell-command (concat "mkdir -p '" org-daynotes-root "'"))
  (find-file (concat org-daynotes-root (string-trim-right
                                        (shell-command-to-string "date '+%F-%A'")))))

(use-package org
  :bind (("C-c o" . org-open-file)
         ("C-c C-o" . org-open-index)
         ("C-c C-c C-o" . org-open-todays-daynote)))
