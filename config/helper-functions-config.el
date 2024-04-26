;;;
;; helper-functions-config.el
;;;

(provide 'helper-functions-config)

(defun dougbeney/set-font (options &optional face)
  (let ((family (plist-get options :family))
        (weight (plist-get options :weight))
        (width (plist-get options :width))
        (height (plist-get options :height))
        (the-face (if face face 'default)))
    (set-face-attribute the-face nil
                        :family (if family family (face-attribute the-face :family))
                        :height (if height height (face-attribute the-face :height))
                        :weight (if weight weight (face-attribute the-face :weight))
                        :width (if width width (face-attribute the-face :width)))))

(defmacro dougbeney/set-font-local (new-face-name options &optional new-face-description)
  (let ((new-face-name (gensym new-face-name))
        (new-face-description
         (if new-face-description
             new-face-description
           "Custom face created by user")))
    `(progn
       (defface ,new-face-name
         '((t ,@options))
         ,new-face-description)
       (buffer-face-set ',new-face-name))))

(lambda ()
  (let ((org-agenda-window-setup 'only-window))
	(org-agenda nil "a")))

(defun dougbeney/shell-cmd-output-completion (prompt shellcmd)
  (let ((selection (split-string (shell-command-to-string shellcmd) "[\n\r]")))
    (completing-read prompt selection)))

(defun dougbeney/terminal ()
  (interactive)
  (split-window nil 8 'above)
  (ansi-term (getenv "SHELL")))

(defun dougbeney/smart-ansi-term ()
  "Open an ansi-term if it doesn't already exist, otherwise switch to current one."
  (interactive)
  (if (get-buffer "*ansi-term*")
    (switch-to-buffer "*ansi-term*")
  (ansi-term (getenv "SHELL"))))

(defun dougbeney/view-blog-posts ()
  (interactive)
  (dired "~/Code/Jekyll/dougie.io/_posts"))

(defun dougbeney/open-terminal-in-workdir ()
  (interactive)
  (call-process-shell-command
   (concat "konsole --workdir " default-directory) nil 0))

(defun dougbeney/neotree-smart-toggle ()
  (interactive)
  (if (string= major-mode "neotree-mode")
      (neotree-toggle)
    (neotree-show)))

(defun dougbeney/neotree-cd-to-pwd ()
  (interactive)
  (neo-global--open-dir default-directory))

(defun dougbeney/neotree-cd-to-pwd-and-show ()
  (interactive)
  (dougbeney/neotree-cd-to-pwd)
  (neotree-show))

(defun dougbeney/neotree-cd-to-code-dir ()
  (interactive)
  (neo-global--open-dir "~/Code"))

(defun dougbeney/set-theme-based-on-mode (&optional source debug)
  (if (and dark-theme-activated
           buffer-file-name
           (string= (file-name-extension buffer-file-name) "md")
           (not (string= (car custom-enabled-themes) light-theme)))
      (progn
        (when debug
          (print source)
          (print buffer-file-name))
        (setq light-theme-activated t) (setq dark-theme-activated nil)
        (enable-theme light-theme))
    (when (and light-theme-activated
               (not (string= major-mode "markdown-mode"))
               (not (string= (car custom-enabled-themes) dark-theme)))
      (progn
        (when debug
          (print source)
          (print buffer-file-name))
        (setq light-theme-activated nil) (setq dark-theme-activated t)
        (enable-theme dark-theme)))))

(defun dougbeney/org-agenda ()
  (interactive)
  (let ((org-agenda-window-setup 'only-window))
	(org-agenda nil)))
