(load "~/.emacs.d/sanemacs.el" nil t)

;;; Your configuration goes below this line.
;;; use-package is already loaded and ready to go!
;;; use-package docs: https://github.com/jwiegley/use-package

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

(defun dougbeney/terminal ()
  (interactive)
  (split-window nil 8 'above)
  (ansi-term "/bin/bash"))

(dougbeney/set-font '(:family "IBM Plex Mono"
                              :height 100))

(setq-default indent-tabs-mode nil)
(setq-default line-spacing 4)

(setq window-divider-default-places t)
(setq window-divider-default-bottom-width 5)
(setq window-divider-default-right-width 5)

(global-set-key [mouse-3] 'mouse-popup-menubar-stuff)
(global-set-key (kbd "C-c o") 'ff-find-other-file)
(global-set-key (kbd "M-7") 'dougbeney/terminal)

(defun dougbeney/whitespace-mode ()
  (interactive)
  (setq-local whitespace-style '(face tabs tab-mark trailing))
  (custom-set-faces
   '(whitespace-tab ((t (:foreground "#636363")))))
  (setq-local whitespace-display-mappings
              '((tab-mark 9 [124 9] [92 9])))
  (whitespace-mode)
                                        ;  (hl-line-mode)
  )

(add-hook 'prog-mode-hook #'dougbeney/whitespace-mode)
(setq-default show-trailing-whitespace nil)

;;; Duplicate line
(global-set-key (kbd "C-c C-d") (kbd "C-a C-SPC C-n M-w C-y C-p C-a"))

(defun dougbeney/edit-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c e") #'dougbeney/edit-emacs-config)

(defun dougbeney/open-terminal-in-workdir ()
  (interactive)
  (call-process-shell-command
   (concat "tilix --working-directory=" default-directory) nil 0))

(global-set-key (kbd "C-c t") 'dougbeney/open-terminal-in-workdir)

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

(use-package term
  :commands ansi-term
  ;;:bind
  ;;;; below is for shell-mode
  ;; (:map shell-mode-map
  ;;       ("C-l" . comint-clear-buffer))
  :hook (term-mode . (lambda ()
                       (dougbeney/set-font-local dougbeney-term-mode-face
                                                 (:family "Ubuntu Mono"
                                                          :height 120))
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

;; (use-package helm
;;   :bind ("M-x" . helm-M-x)
;;   :bind ("C-x b" . helm-buffers-list)
;;   :bind ("C-x C-f" . helm-find-files)
;;   :bind ("C-x r b" . helm-filtered-bookmarks)
;;   :init
;;   (require 'helm-config)
;;   :config
;;   (setq helm-posframe-parameters
;;         '((left-fringe . 10)
;;           (right-fringe . 10)))
;;   (helm-posframe-enable))

(use-package ivy
  :config
  (use-package counsel)
  (use-package ivy-posframe)
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-mode 1)
  (ivy-posframe-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
;  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package helm-swoop
  :bind ("M-i" . helm-swoop)
  :bind ("C-x M-i" . helm-multi-swoop)
  :bind ("C-c M-i" . helm-multi-swoop-all))

(use-package helm-projectile
  :after (projectile)
  :config
  (helm-projectile-on))

(use-package autopair
  :config
  (autopair-global-mode))

(use-package elpy
  :init
  (elpy-enable))

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
  :mode "\\.php\\'")

(use-package vue-mode
  :mode "\\.vue\\'")

(use-package sass-mode
  :mode "\\.s?css\\'")

(use-package emmet-mode
  :hook (web-mode . emmet-mode))

;; (use-package vue-mode
;;   :config
;;   (setq mmm-submode-decoration-level 0))

;;; Avy, an alternative to ace
(use-package avy
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-\"") 'avy-goto-char-2)
  (global-set-key (kbd "M-g g") 'avy-goto-line))

;; (use-package doom-modeline
;;   :init
;;   (doom-modeline-mode 1)
;;   (use-package all-the-icons))

(use-package telephone-line
  :config
  (telephone-line-mode 1))

(use-package all-the-icons)

(set-frame-parameter nil 'fullscreen 'maximized)

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq js2-strict-missing-semi-warning nil))
