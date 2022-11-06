;;;
;; org-config.el
;; Org-Mode Configuration
;;;

(provide 'org-config)
(require 'helper-functions-config)

(setq org-root (concat (shell-command-to-string "printf $HOME") "/Sync/Org/"))
(setq org-daynotes-root (concat org-root "Day Notes/"))
(setq org-weeknotes-root (concat org-root "Week Notes/"))
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

(defun org-open-weeknote ()
  (interactive)
  (let* ((week-date (string-trim-right
                       (shell-command-to-string "date +%Y-%m-week-%U")))
         (week-note-fname (concat week-date ".org"))
         (week-note (concat org-weeknotes-root week-note-fname)))

    (shell-command (concat "mkdir -p '" org-weeknotes-root "'"))

    (when (not (file-exists-p week-note))
      (shell-command (concat "echo '#+title: " week-date " Notes\n\n* ' > '" week-note "'")))

    (find-file (concat org-weeknotes-root week-note-fname ))))

(defun org-open-next-weeknote ()
  (interactive)
  (let* ((week-date (string-trim-right
                     (shell-command-to-string "date -d \"next monday\" +%Y-%m-week-2%U")))
         (week-note-fname (concat week-date ".org"))
         (week-note (concat org-weeknotes-root week-note-fname)))

    (shell-command (concat "mkdir -p '" org-weeknotes-root "'"))

    (when (not (file-exists-p week-note))
      (shell-command (concat "echo '#+title: " week-date " Notes\n\n* ' > '" week-note "'")))

    (find-file (concat org-weeknotes-root week-note-fname ))))

(use-package org
  :bind (("C-c o" . org-open-file)
         ("C-c C-o" . org-open-home)
         ("C-c \\" . org-open-todays-daynote)
         ("C-c C-x \\" . org-open-weeknote)
         ("C-c C-x C-\\" . org-open-weeknote)
         ("C-c C-\\" . org-open-root-dir))
  :config
  (setq-default org-src-preserve-indentation t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (emacs-lisp . t))))

;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode))
