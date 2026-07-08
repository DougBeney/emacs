;;;
;; org-config.el
;; Org-Mode Configuration
;;;

(provide 'org-config)
(require 'helper-functions-config)

(setq org-root (concat (shell-command-to-string "printf $HOME") "/Documents/Org/"))
(setq org-daynotes-root (concat org-root "Day Notes/"))
(setq org-weeknotes-root (concat org-root "Week Notes/"))
(setq org-home-file (concat org-root "Home.org"))
(setq org-agenda-skip-scheduled-if-done t)

(setq org-agenda-files
      '("~/Documents/Org/Main/Events.org"
        "~/Documents/Org/TODO/Inbox.org"
        "~/Documents/Org/TODO/Work.org"
        "~/Documents/Org/TODO/Personal.org"
        "~/Documents/Org/TODO/Sprint.org"
        "~/Documents/Org/TODO/Archive.org"
        ))

(setq org-refile-targets
      '((org-agenda-files :maxlevel . 1)
        ("~/Documents/Org/TODO/Projects.org"
         :maxlevel . 1)))

(setq org-capture-templates
      '(("t" "Personal TODO" entry (file+headline "~/Documents/Org/TODO/Inbox.org" "Personal")
         "* TODO %?")
        ("w" "Work TODO" entry (file+headline "~/Documents/Org/TODO/Inbox.org" "Work")
         "* TODO %?")
        ("e" "Event" entry (file+headline "~/Documents/Org/Main/Events.org" "Reminders")
         "* %?\nSCHEDULED: %^T\n")
        ("s" "Sprint daynote" entry (file+headline "~/Documents/Org/TODO/Sprint.org" "Daynotes")
         "* %t %?" :prepend t)
        ("1" "10x journal entry" entry (file+headline "~/Documents/Org/Main/10x.org" "10x Journal")
         "* %U\n%?" :prepend t)
        ))

;; Most importantly, this will show holidays in org-mode
(setq org-agenda-include-diary t)

(setq-default org-image-actual-width nil)

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
  :bind      (("C-c o" . org-roam-node-find)
          ("C-c C-o" . org-capture)
          ("C-M-c" . org-capture)
          ("C-c \\" . dougie-org-open-journal)
          ("C-c C-\\" . dougie-org-open-root-dir)
          ("C-c C-b" . dougie-org-open-business-ideas)
          (:map org-mode-map
                ("C-c l" . org-roam-node-insert)))
  :hook (org-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (when (require 'evil nil t)
    (evil-define-key 'normal org-mode-map
      (kbd "t") #'org-todo)
    (evil-define-key 'normal org-mode-map
      (kbd "+") #'org-priority-up
      (kbd "-") #'org-priority-down))
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (setq org-startup-indented t)
  (define-key org-mode-map (kbd "C-c C-r") 'org-refile)
  (setq-default org-src-preserve-indentation t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t) (emacs-lisp . t))))

(use-package org-agenda
  :ensure nil
  :straight nil
  :bind ((:map org-agenda-mode-map
               ("c" . org-capture)))
  :hook (org-agenda-mode . hl-line-mode))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)))

(use-package org-download)
(use-package org-contacts)

(use-package org-roam
  :config
  (setq org-roam-directory (file-truename "~/Documents/Org"))
  (org-roam-db-autosync-mode))

(add-hook 'org-mode-hook 'flyspell-mode)

(defun org-refile-any ()
  "Refile to *any* subtree (deep search), without modifying global settings."
  (interactive)
  (let ((org-refile-targets '((nil :maxlevel . 10)
                              (org-agenda-files :maxlevel . 10)))
        (org-refile-use-outline-path 'full)
        (org-outline-path-complete-in-steps nil))
    (org-refile)))

(defun dougbeney/org-agenda ()
  (interactive)
  (let ((org-agenda-window-setup 'only-window))
    (org-agenda nil)))

;; KEYBINDINGS
(global-set-key (kbd "C-M-o") #'dougbeney/org-agenda)
