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

(setq-default org-startup-truncated nil)

(setq-default org-startup-folded 'show2levels)

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

(defun org-open-journal ()
  (interactive)
  (find-file (concat org-root "Journal.org")))

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

(defun org-open-business-ideas ()
  (interactive)
  (find-file (concat org-root "business-ideas.org")))

(use-package org
  :bind (("C-c o" . org-open-file)
         ("C-c C-o" . org-open-home)
         ("C-c C-\\" . org-open-journal)
         ("C-c \\" . org-open-root-dir)
		 ("C-c C-b" . org-open-business-ideas))
  :config
  (use-package org-bullets)
  (define-key org-mode-map (kbd "C-c C-r") 'org-refile)
  (setq-default org-src-preserve-indentation t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (emacs-lisp . t))))

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'org-bullets-mode)

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.7))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
