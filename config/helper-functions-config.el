;;;
;; helper-functions-config.el
;;;

(provide 'helper-functions-config)

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

(defun dougbeney/open-terminal-in-workdir ()
  (interactive)
  (call-process-shell-command
   (concat "kitty \"" (expand-file-name default-directory) "\"") nil 0))

;; TODO: Update blog and refresh this command
(defun dougbeney/view-blog-posts ()
  (interactive)
  (dired "~/Code/Jekyll/dougie.io/_posts"))

(defun dougbeney/neotree-smart-toggle ()
  (interactive)
  (if (string= major-mode "neotree-mode")
      (neotree-toggle)
    (neotree-show)))

(defun dougbeney/treemacs-smart-toggle ()
  (interactive)
  (if (string= major-mode "treemacs-mode")
	  (treemacs)
	(treemacs-select-window)))

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
