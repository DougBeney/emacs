;;;
;; workflow-config.el
;; Various packages that enhances the workflow of Emacs.
;;;

(provide 'workflow-config)

(require 'helper-functions-config)

;; Ivy - Provides a nifty auto-complete for finding files and shit.
;; Counsel - Adds Ivy completion for other Emacs shit. Ex. Viewing buffer list.
;; Swiper - Better search for searching for text in document. C-s
;; Note: This is NOT code auto-completion. Refer to company for that.
(use-package counsel
  :config
  (ivy-mode 1)
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (use-package ivy-posframe)
  (ivy-posframe-mode 1)
  ;; (setq ivy-use-virtual-buffers t)
  ;; (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; Company is for code auto-completion
(use-package company
  :hook (after-init . global-company-mode)
  :bind ("M-/" . company-complete)
  :config
  (setq company-idle-delay 0))

;; Snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; Project management
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
  (setq projectile-switch-project-action 'neotree-projectile-action))

;; Jump to a character. Avy is an alternative to ace.
(use-package avy
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-\"") 'avy-goto-char-2)
  (global-set-key (kbd "M-g g") 'avy-goto-line))

;; Workspaces
(use-package eyebrowse
  :diminish eyebrowse-mode
  :config (progn
            (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
            (eyebrowse-mode t)
            (setq eyebrowse-new-workspace t)))

;; Automatically closes parenthesis, brackets, and quotes
(use-package autopair
  :config
  (autopair-global-mode))

;; git related packages
(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))
(use-package diff-hl
  :requires magit
  :hook ((after-init . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))
