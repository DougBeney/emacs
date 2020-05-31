(load "~/.emacs.d/sanemacs.el" nil t)
(load "~/.emacs.d/helper-functions.el" nil t)

(section ; No-config packages
 (use-package all-the-icons :defer t)
 (use-package cyberpunk-theme :defer t))

(section ; Top-Level Config

 (setenv "PATH" (concat (getenv "PATH") (shell-command-to-string "echo $PATH")))
 (setq exec-path (append exec-path (split-string (shell-command-to-string "echo $PATH"))))

 (setq dark-theme 'cyberpunk
       light-theme 'adwaita)

 (load-theme dark-theme)
 (load-theme light-theme t t)

 (setq dark-theme-activated t
       light-theme-activated nil)

 (add-hook 'pre-command-hook
           (lambda ()
             (dougbeney/set-theme-based-on-mode "pre command hook")))

 (add-hook 'after-change-major-mode-hook
           (lambda ()
             (dougbeney/set-theme-based-on-mode "after change major mode hook")))

 ;; Set the font
 (dougbeney/set-font '(:family "IBM Plex Mono"
                               :height 90))

 (setq-default indent-tabs-mode nil)
 (setq-default line-spacing 0)

 (setq window-divider-default-places t)
 (setq window-divider-default-bottom-width 5)
 (setq window-divider-default-right-width 5))

(section ; Global Key Bindings
 ;; Right click
 (global-set-key [mouse-3] 'mouse-popup-menubar-stuff)

 ;; Find other file. Useful for C/C++
 (global-set-key (kbd "C-c o") 'ff-find-other-file)

 ;; Open terminal on bottom of screen
 (global-set-key (kbd "M-7") 'dougbeney/terminal)

 ;;; Duplicate line
 (global-set-key (kbd "C-c C-d") (kbd "C-a C-SPC C-n M-w C-y C-p C-a"))

 (global-set-key (kbd "C-c e") #'dougbeney/edit-emacs-config)

 (global-set-key (kbd "C-c t") #'dougbeney/open-terminal-in-workdir))

;;; Open blog post directory
(global-set-key (kbd "C-c b") #'dougbeney/view-blog-posts)
(global-set-key (kbd "C-c r") #'reload-config)

(section ; UI / UX
 (use-package dashboard
   :if (< (length command-line-args) 2)
   :config
   (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
   (setq dashboard-banner-logo-title "Welcome to Sanemacs, Doug!")
   (setq dashboard-center-content t)
   (setq dashboard-startup-banner "~/.emacs.d/sanemacs-logo.png")
   (setq dashboard-items '((agenda . 5)
                           (recents  . 5)
                           (projects . 15)
                           (bookmarks . 5)))
   (dashboard-setup-startup-hook))

 (use-package neotree
   :bind ("M-0" . dougbeney/neotree-smart-toggle)
   :bind ("M-9" . dougbeney/neotree-cd-to-pwd-and-show)
   :bind ("M-8" . dougbeney/neotree-cd-to-code-dir)
   :hook (neotree-mode . (lambda ()
                           (setq-local line-spacing 10)
                           (setq-local buffer-face-mode-face '(:family "Fira Sans" :height 100 :weight 'normal))
                           (buffer-face-mode)))
   :config
   (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
   (setq neo-window-fixed-size nil)
   (setq projectile-switch-project-action 'neotree-projectile-action)))

(section ; Sane Enhancements
 ;; Show tabs and trailing whitespace
 (defun dougbeney/whitespace-mode ()
   (interactive)
   (setq-local whitespace-style '(face tabs tab-mark trailing))
   (custom-set-faces
    '(whitespace-tab ((t (:foreground "#636363")))))
   (setq-local whitespace-display-mappings '((tab-mark 9 [124 9] [92 9])))
   (whitespace-mode))

 ;; (add-hook 'prog-mode-hook #'dougbeney/whitespace-mode)
 (setq-default show-trailing-whitespace nil))

(use-package term
  :commands ansi-term
  :hook (term-mode . (lambda ()
                       (dougbeney/set-font-local dougbeney-term-mode-face
                                                 (:family "Ubuntu Mono"
                                                          :height 90))
                       (setq-local line-spacing 0)
                       (setq-local mode-line-format nil)))
  :config
  (setq comint-scroll-to-bottom-on-input t))

(use-package treemacs
  :bind ("C-c M-0" . treemacs)
  :config
  (use-package treemacs-projectile))

(use-package company
  :hook (after-init . global-company-mode)
  :bind ("M-/" . company-complete)
  :config
  (setq company-idle-delay 0))

(use-package projectile
  :init
  :config
  (setq projectile-globally-ignored-files (list "db.sqlite3"))
  (setq projectile-globally-ignored-directories
        (list ".venv" "node_modules"))
  (setq projectile-globally-ignored-file-suffixes
        (list "sqlite" "sqlite3"))
  (projectile-mode +1)
  (setq projectile-project-search-path (cddr (directory-files "~/Code" t)))
  (define-key projectile-mode-map (kbd "M-m") 'projectile-command-map))

(use-package ivy
  :config
  (use-package counsel)
  ;;(use-package ivy-posframe)
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-mode 1)
  ;; (ivy-posframe-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package autopair
  :config
  (autopair-global-mode))

(use-package elpy
  :init
  (elpy-enable)
  (setq elpy-rpc-virtualenv-path 'default)
  (setq elpy-rpc-python-command "python3"))

(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package diff-hl
  :requires magit
  :hook ((after-init . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package eyebrowse
  :diminish eyebrowse-mode
  :config (progn
            (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
            (eyebrowse-mode t)
            (setq eyebrowse-new-workspace t)))

(use-package web-mode
  :mode "\\.html?\\'"
  :mode "\\.vue\\'")

(use-package php-mode
  :mode "\\.php\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package markdown-mode
  :mode "//.md\\'"
  :mode "\\.markdown\\'"
  :hook (markdown-mode . (lambda ()
                           (olivetti-mode 1)
                           (dougbeney/set-font-local dougbeney-term-mode-face
                                                     (:family "Liberation Serif"
                                                              :height 120))
                           (fringe-mode -1)))
  :init
  (setq markdown-header-scaling t))

(use-package olivetti
  :init
  (setq-default olivetti-body-width 140))

(use-package sass-mode
  :mode "\\.scss\\'"
  :mode "\\.sass\\'")

(use-package emmet-mode
  :hook (web-mode . emmet-mode))

;;; Avy, an alternative to ace
(use-package avy
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-\"") 'avy-goto-char-2)
  (global-set-key (kbd "M-g g") 'avy-goto-line))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (use-package all-the-icons))

(use-package lsp-mode
  :init
  (setq-default lsp-keymap-prefix "s-l")
  :config
  (use-package company-lsp
    :config
      (push 'company-lsp company-backends))
  :hook ((php-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

(use-package which-key
  :config
  (which-key-mode))
