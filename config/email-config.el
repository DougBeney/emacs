;;; email-config.el -*- lexical-binding: t; -*-
;;
;; mu4e config
;; make sure "mu" is installed on system...it'll bring along the mu4e emacs package
;;

(require 'cl-lib)
(provide 'email-config)


(defcustom dougbeney/email-accounts
  '((:name "Personal" :email "someemail@domain.com")
    (:name "Work" :email "work-email@domain.com"))
  "List of email accounts for mu4e contexts."
  :type '(repeat (plist :key-type symbol :value-type string))
  :group 'mu4e)

(defcustom dougbeney/mu4e-bookmarks
  '((:name "Example Inbox"
     :query "maildir:/example/Inbox OR maildir:/example2/Inbox"
     :key ?u))
  "List of mu4e bookmarks."
  :type '(repeat (plist :key-type symbol :value-type sexp))
  :group 'mu4e)

(cl-defun dougie/mu4e-context (&key name dir email full-name)
  "Create a mu4e context with sane defaults."
  (let* ((name (or name email))
         (dir (or dir (concat "/" (replace-regexp-in-string " " "_" (downcase name)))))
         (full-name (or full-name "Dougie Beney")))
    (make-mu4e-context
     :name name
     :match-func
     (lambda (msg)
       (when msg
         (when-let ((maildir (mu4e-message-field msg :maildir)))
           (string-prefix-p dir maildir))))
     :vars `(
             (user-mail-address . ,email)
             (user-full-name    . ,full-name)

             (mu4e-sent-folder   . ,(concat dir "/Sent"))
             (mu4e-drafts-folder . ,(concat dir "/Drafts"))
             (mu4e-trash-folder  . ,(concat dir "/Trash"))
             (mu4e-refile-folder
              . ,(let ((archive-dir dir))
                   (lambda (msg)
                     (let* ((date (mu4e-message-field msg :date))
                            (year (format-time-string "%Y" date)))
                       (concat archive-dir "/Archive/" year)))))
             ))))

; Newer transient package required for mu4e
(use-package transient :ensure t)

(use-package mu4e
  :after transient
  :ensure nil
  :straight nil
    :bind
    (:map mu4e-view-mode-map
          ("e" . #'mu4e-view-save-attachments))
    :hook ((mu4e-main-mode . (lambda () (display-line-numbers-mode -1)))
           (mu4e-headers-mode . (lambda () (display-line-numbers-mode -1)))
           (mu4e-compose-mode . (lambda () (display-line-numbers-mode -1)))
           (mu4e-view-mode . (lambda () (display-line-numbers-mode -1)))
           (mu4e-view-mode . (lambda ()
                               (when (require 'evil nil t)
                                 (evil-local-set-key 'normal (kbd "e")
                                  #'mu4e-view-save-attachments))))
           )
    :custom
    ;; Receiving email
    (mu4e-get-mail-command "mbsync --all")
    (mu4e-update-interval 300)

    ;; Sending email
    (message-send-mail-function 'sendmail-send-it)
    (sendmail-program "msmtp")
    (message-sendmail-f-is-evil t)
    (message-sendmail-extra-arguments '("--read-envelope-from"))
    (message-sendmail-envelope-from 'header)
    (message-cite-reply-position 'above)
    (message-citation-line-format "\n\nOn %a %d %b %Y at %R, %f wrote:\n")
    (message-citation-line-function 'message-insert-formatted-citation-line)

    ;; General usability
    (mu4e-context-policy 'pick-first)
    (mu4e-compose-context-policy 'ask)
    (mu4e-change-filenames-when-moving t)

    ;; HTML rendering
    (shr-color-visible-luminance-min 70) ;; better contrast
    (shr-use-colors nil)                 ;; strip garish inline colors
    (shr-use-fonts nil)                  ;; keep your monospace consistent
    (shr-max-image-proportion 0.5)       ;; don't let images blow up
    (mu4e-headers-visible-columns (lambda () (/ (frame-width) 2)))

    (mu4e-maildir-shortcuts
     '(
       (:maildir "/personal/Inbox" :key  ?i)
       (:maildir "/work/Inbox" :key  ?w)
       (:maildir "/newsletters/Inbox" :key  ?n)))

    (mu4e-split-view 'vertical)

    (mu4e-attachment-dir "/home/doug/Downloads")

    (setq mu4e-bookmarks dougie/mu4e-bookmarks)

    :config
    (setq mu4e-contexts
      (mapcar (lambda (account)
                (dougie/mu4e-context
                 :name (plist-get account :name)
                 :email (plist-get account :email)))
              dougie/email-accounts)))
