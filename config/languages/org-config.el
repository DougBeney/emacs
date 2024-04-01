;;;
;; org-config.el
;; Org-Mode Configuration
;;;

(provide 'org-config)
(require 'helper-functions-config)

(setq org-root (concat (shell-command-to-string "printf $HOME") "/Sync/Org/"))
(setq org-daynotes-root (concat org-root "Day Notes/"))
(setq org-weeknotes-root (concat org-root "Week Notes/"))
(setq org-home-file (concat org-root "Home.org"))

(setq org-agenda-files '("~/Sync/Org/TODO.org" "~/Sync/Org/Work.org" "~/Sync/Org/Personal.org" "~/Sync/Org/Events.org" "~/Sync/Org/Journal.org"))

(add-hook 'org-agenda-finalize-hook #'hl-line-mode)

(setq-default org-startup-truncated nil)

(setq-default org-startup-folded 'show2levels)

(defun dougie-org-open-root-dir ()
  (interactive)
  (dired org-root))

(defun dougie-org-open-home ()
  (interactive)
  (find-file org-home-file))

(defun dougie-org-open-file ()
  (interactive)
  (let ((selected_file
		(dougbeney/shell-cmd-output-completion
		 "Find Org file: "
		 (concat "find '" org-root "' -type f -iname '*.org'"))))
	(find-file selected_file)))

(defun dougie-org-open-journal ()
  (interactive)
  (find-file (concat org-root "Journal.org")))

(defun dougie-org-open-todays-daynote ()
  (interactive)
  (let* ((todays-date (string-trim-right
                       (shell-command-to-string "date '+%F-%A'")))
         (todays-daynote-fname (concat todays-date ".org"))
         (todays-daynote (concat org-daynotes-root todays-daynote-fname)))

    (shell-command (concat "mkdir -p '" org-daynotes-root "'"))

    (when (not (file-exists-p todays-daynote))
      (shell-command (concat "echo '#+title: " todays-date " Notes\n\n* ' > '" todays-daynote "'")))

    (find-file (concat org-daynotes-root todays-daynote-fname ))))

(defun dougie-org-open-weeknote ()
  (interactive)
  (let* ((week-date (string-trim-right
                       (shell-command-to-string "date +%Y-%m-week-%U")))
         (week-note-fname (concat week-date ".org"))
         (week-note (concat org-weeknotes-root week-note-fname)))

    (shell-command (concat "mkdir -p '" org-weeknotes-root "'"))

    (when (not (file-exists-p week-note))
      (shell-command (concat "echo '#+title: " week-date " Notes\n\n* ' > '" week-note "'")))

    (find-file (concat org-weeknotes-root week-note-fname ))))

(defun dougie-org-open-next-weeknote ()
  (interactive)
  (let* ((week-date (string-trim-right
                     (shell-command-to-string "date -d \"next monday\" +%Y-%m-week-2%U")))
         (week-note-fname (concat week-date ".org"))
         (week-note (concat org-weeknotes-root week-note-fname)))

    (shell-command (concat "mkdir -p '" org-weeknotes-root "'"))

    (when (not (file-exists-p week-note))
      (shell-command (concat "echo '#+title: " week-date " Notes\n\n* ' > '" week-note "'")))

    (find-file (concat org-weeknotes-root week-note-fname ))))

(defun dougie-org-open-business-ideas ()
  (interactive)
  (find-file (concat org-root "business-ideas.org")))

(use-package org
  :bind  (("C-c o" . dougie-org-open-file)
         ("C-c C-o" . dougie-org-open-home)
         ("C-c \\" . dougie-org-open-journal)
         ("C-c C-\\" . dougie-org-open-root-dir)
		 ("C-c C-b" . dougie-org-open-business-ideas))
  :hook (org-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (setq org-startup-indented t)
  (use-package org-superstar)
  (define-key org-mode-map (kbd "C-c C-r") 'org-refile)
  (setq-default org-src-preserve-indentation t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (emacs-lisp . t))))

(add-hook 'org-mode-hook 'flyspell-mode)

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.35))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.25))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.2)))))
