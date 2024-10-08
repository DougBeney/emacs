;;
;; workflow-config.el
;; Various packages that enhances the workflow of Emacs.
;;;

(provide 'workflow-config)

(require 'helper-functions-config)

;; (setq desktop-path '("~/.emacs.d/"))
;(desktop-save-mode 1)
(electric-pair-mode t)

(setq history-delete-duplicates t)

(defun my/focus-new-client-frame ()
  (select-frame-set-input-focus (selected-frame)))
(add-hook 'server-after-make-frame-hook #'my/focus-new-client-frame)

;;evil - Vim keybindings for Emacs
;; (use-package evil
;;   :inithh
;;   (setq evil-want-keybinding nil)
;;   :config
;;   (evil-mode 1)
;;   (use-package evil-collection
;; 	:init
;; 	(evil-collection-init))
;;   (use-package evil-surround
;; 	:config
;; 	(global-evil-surround-mode 1)))

;; Ivy - Provides a nifty auto-complete for finding files and shit.
;; Counsel - Adds Ivy completion for other Emacs shit. Ex. Viewing buffer list.
;; Swiper - Better search for searching for text in document. C-s
;; Note: This is NOT code auto-completion. Refer to company for that.
;; (use-package counsel ;; ivy
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-height 25)
;;   (use-package ivy-posframe)
;;   ;(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;;   ;(ivy-posframe-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq enable-recursive-minibuffers t)
;;   (global-set-key "\C-s" 'swiper)
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   (global-set-key (kbd "C-c k") 'counsel-ag)
;;   (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package helm
  :bind (("M-x" . helm-M-x)
		 ("C-x C-f" . helm-find-files)
		 ("C-s" . helm-occur))
  :config
  (setq completion-styles '(flex))
  (setq helm-move-to-line-cycle-in-source nil)
  :init
  (helm-mode 1))

(use-package helm-projectile)

;; Company is for code auto-completion
(use-package company
  :hook (after-init . global-company-mode)
  :bind ("M-/" . company-complete)
  :bind ("C-x C-/" . dabbrev-expand)
  :config
  (setq company-global-modes '(not org-mode))
  (setq company-idle-delay 0)
  (setq company-show-numbers t))

(use-package editorconfig
  :config
  (editorconfig-mode 1))
;; Snippets
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

;; Project management
(use-package projectile
  :init
  :config
  (setq
   projectile-indexing-method 'hybrid
   projectile-globally-ignored-files '("db.sqlite3")
   projectile-globally-ignored-file-suffixes '("sqlite" "sqlite3")
   projectile-project-search-path (cddr (directory-files "~/Code" t))
   projectile-auto-discover nil)

  (setq projectile-globally-ignored-directories
		(append projectile-globally-ignored-directories '("*.venv" "*node_modules" "*dist" "*__pycache__" "*migrations")))

  (projectile-mode +1)

  (define-key projectile-mode-map (kbd "M-m") 'projectile-command-map))

(use-package neotree
  ;; :bind ("M-0" . dougbeney/neotree-smart-toggle)
  :bind ("M-9" . dougbeney/neotree-cd-to-pwd-and-show)
  :bind ("M-8" . dougbeney/neotree-cd-to-code-dir)
  :hook (neotree-mode . (lambda ()
                          (setq-local line-spacing 5)
                          (setq-local buffer-face-mode-face '(:family "Fira Sans" :height 100 :weight 'normal))
                          (buffer-face-mode)))
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-window-fixed-size nil)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (defun neo-path--insert-header-buttonized (path) nil)
  (setq neo-banner-message " "))

(use-package treemacs
  :bind ("M-0" . dougbeney/treemacs-smart-toggle))

(use-package ace-window
  :bind ("C-x o" . ace-window))

;; Jump to a character. Avy is an alternative to ace.
(use-package avy
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-\"") 'avy-goto-char-2)
  (global-set-key (kbd "M-g g") 'avy-goto-line))

(use-package tab-bar
  :ensure nil
  :bind (("M-1" . 'tab-bar-select-tab)
		 ("M-2" . 'tab-bar-select-tab)
		 ("M-3" . 'tab-bar-select-tab)
		 ("M-4" . 'tab-bar-select-tab)
		 ("M-5" . 'tab-bar-select-tab)
		 ("M-6" . 'tab-bar-select-tab)
		 ("M-7" . 'tab-bar-select-tab)
		 ("M-8" . 'tab-bar-select-tab)
		 ("M-9" . 'tab-bar-select-tab)
		 ("M-9" . 'tab-bar-select-tab)
		 ("M-t" . 'tab-bar-new-tab)
		 ("M-[" . 'tab-previous)
		 ("M-]" . 'tab-next)
		 ("M-C-[" . 'dougie-tab-move-left)
		 ("M-C-]" . 'dougie-tab-move-right))
  :init
  (defun dougie-tab-move-right () (interactive) (tab-move 1))
  (defun dougie-tab-move-left () (interactive) (tab-move -1))
  :config
  (setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
  (setq tab-bar-close-button-show nil)       ;; hide tab close / X button
  (setq tab-bar-new-tab-choice "*scratch*");; buffer to show in new tabs
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

  (setq tab-bar-select-tab-modifiers "M"))

;; git related packages
(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package diff-hl
  :requires magit
  :hook ((after-init . global-diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package origami)

(use-package expand-region
  :bind ("C-;" . er/expand-region))

(defun surround2 (begin end open close)
  "Put OPEN at START and CLOSE at END of the region.
If you omit CLOSE, it will reuse OPEN."
  (interactive  "r\nsStart: \nsEnd: ")
  (when (string= close "")
    (setq close open))
  (save-excursion
    (goto-char end)
    (insert close)
    (goto-char begin)
    (insert open)))

(defun surround (begin end char)
  "Put OPEN at START and CLOSE at END of the region.
If you omit CLOSE, it will reuse OPEN."
  (interactive  "r\nsSurround with: ")
  (surround2 begin end char char))
