;; -*- lexical-binding: t; -*-

;;;
;; fun-config.el
;; Misc fun, entertaining, or not-directly-programming-related Emacs packages
;;;

(provide 'fun-config)

(use-package eww
  :ensure nil
  :straight nil
  :init
  (setq eww-search-prefix "https://lite.duckduckgo.com/lite/?q="))

(use-package csv-mode
  :mode "\\.csv\\'"
  :hook (csv-mode . csv-align-mode))

;; (use-package activity-watch-mode
;;   :init
;;   (global-activity-watch-mode))

(use-package calc
    :ensure nil
    :custom
    (calc-display-trail nil))

(defun marketing/lucidlink-links-format ()
  (interactive)
  (replace-string
   "/media/ltv/" "Lucid: "
   nil
   (point-min)
   (point-max)))

(defun marketing/copysail-links-format ()
  (interactive)
  (replace-string
   "&approved=not_approved" ""
   nil
   (point-min)
   (point-max))
  (replace-string
   "https://copysail.com" "copySail: https://copysail.com"
   nil
   (point-min)
   (point-max))
  (replace-string
   "copySail: copySail: https://copysail.com" "copySail: https://copysail.com"
   nil
   (point-min)
   (point-max)))



;; Credit line for diomal
(defun diomal-cred ()
  (interactive)
  (insert ", edited by Diomal"))
