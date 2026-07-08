;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sanemacs version 1.0.0 (BETA) (straight.el) ;;;
;;; https://sanemacs.com                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For performance
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))


(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;;; Disable menu-bar, tool-bar, and scroll-bar.
(if (fboundp 'menu-bar-mode)
    (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

;;; Fix this bug:
;;; https://www.reddit.com/r/emacs/comments/cueoug/the_failed_to_download_gnu_archive_is_a_pretty/
(when (version< emacs-version "26.3")
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;;; Setup straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Setup use-package
(setq-default straight-use-package-by-default t)
(straight-use-package 'use-package)
(setq use-package-always-ensure t)

(defvar sanemacs-disable-package-enable-at-startup-warning nil
	"If non-nil, suppress the sanemacs early-init package warning.")

;; Disable
(let* ((dir user-emacs-directory)
	   (file (expand-file-name "early-init.el" dir))
	   (needle "(setq package-enable-at-startup nil)"))
	(cond
	 ;; Case 1: file does not exist → create it
	 ((not (file-exists-p file))
	  (with-temp-file file
		(insert needle "\n")))

	 ;; Case 2: file exists but missing the setting → warn
	 ((and (not sanemacs-disable-package-enable-at-startup-warning)
		   (with-temp-buffer
			 (insert-file-contents file)
			 (not (search-forward "package-enable-at-startup" nil t))))
	  (display-warning
	   'sanemacs
	   (concat
		"early-init.el exists but does not set "
		"`package-enable-at-startup` to nil.\n"
		"This is recommended to prevent package.el from loading at startup.\n\n"
		"To suppress this warning, set:\n"
		"(setq sanemacs-disable-package-enable-at-startup-warning t)")
	   :warning))))

;;; Useful Defaults
(setq-default cursor-type '(bar . 2))          ; Line-style cursor similar to other text editors
(setq inhibit-startup-screen t)           ; Disable startup screen
(setq initial-scratch-message "")         ; Make *scratch* buffer blank
(setq-default frame-title-format '("%b")) ; Make window title the buffer name
(setq ring-bell-function 'ignore)         ; Disable bell sound
(fset 'yes-or-no-p 'y-or-n-p)             ; y-or-n-p makes answering questions faster
(show-paren-mode 1)                       ; Show closing parens by default
(setq linum-format "%4d ")                ; Line number format
(delete-selection-mode 1)                 ; Selected text will be overwritten when you start typing
(global-auto-revert-mode t)               ; Auto-update buffer if file has changed on disk
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)    ; Delete trailing whitespace on save

(add-hook 'prog-mode-hook
          (if (or
			   ; If linum-mode doesn't exist...
			   (not (fboundp 'linum-mode))
			   ; ...or Emacs has display-line-numbers-mode capability
			   (and (fboundp 'display-line-numbers-mode) (display-graphic-p)))
			  ; ...then use display-line-numbers-mode!
              'display-line-numbers-mode
			; Otherwise, use linum-mode
            'linum-mode))

(defun sanemacs/backward-kill-word ()
  (interactive "*")
  (push-mark)
  (backward-word)
  (delete-region (point) (mark)))

;;; Keybindings
(global-set-key [mouse-3] 'mouse-popup-menubar-stuff)          ; Gives right-click a context menu
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop) ; Indent selection by one tab length
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)  ; De-indent selection by one tab length
(global-set-key (kbd "M-DEL") 'sanemacs/backward-kill-word)    ; Kill word without copying it to your clipboard
(global-set-key (kbd "C-DEL") 'sanemacs/backward-kill-word)    ; Kill word without copying it to your clipboard
(global-set-key (kbd "C-/") 'undo-only)                        ; Undo
(global-set-key (kbd "C-S-/") 'undo-redo)                      ; Redo

;;; Offload the custom-set-variables to a separate file
;;; This keeps your init.el neater and you have the option
;;; to gitignore your custom.el if you see fit.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;;; Load custom file. Don't hide errors. Hide success message
(with-eval-after-load (expand-file-name custom-file)
  (if (not custom-enabled-themes)
    (load-theme 'wheatgrass t)))
(load custom-file nil t)

;;; Put Emacs auto-save and backup files to /tmp/ or C:/Temp/
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq
   backup-by-copying t                                        ; Avoid symlinks
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   auto-save-list-file-prefix emacs-tmp-dir
   auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t))  ; Change autosave dir to tmp
   backup-directory-alist `((".*" . ,emacs-tmp-dir)))

;;; Lockfiles unfortunately cause more pain than benefit
(setq create-lockfiles nil)

;;; Load wheatgrass as the default theme if one is not loaded already

(defun reload-config ()
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))
