;; -*- lexical-binding: t; -*-

;;
;; workflow-config.el
;; Various packages that enhances the workflow of Emacs.
;;;

(provide 'workflow-config)

(require 'helper-functions-config)

(electric-pair-mode t)

(setq history-delete-duplicates t)

;; Dired customizations
(add-hook 'dired-mode-hook #'dired-hide-details-mode)
(add-hook 'dired-mode-hook (lambda () (display-line-numbers-mode -1)))

(setq-default dired-listing-switches "-alg --group-directories-first")
(use-package dired-subtree
    :after dired
    :commands (dired-subtree-toggle dired-subtree-remove dired-subtree-cycle)
    :bind
    (:map dired-mode-map
        ;; TAB toggles subtree only in Dired buffers
        ("<tab>" . dired-subtree-toggle)))

;; Have to see if I still need this
;; (defun my/focus-new-client-frame ()
;;   (select-frame-set-input-focus (selected-frame)))
;; (add-hook 'server-after-make-frame-hook #'my/focus-new-client-frame)

;;evil - Vim keybindings for Emacs
(use-package evil
  :init
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-keybinding nil)
  (setq evil-kill-on-visual-paste nil)
  :config
  (evil-mode 1)
  (add-hook 'git-commit-mode-hook #'evil-insert-state))

(use-package evil-collection
  :after org
  :init
  (evil-collection-init))

(use-package evil-org
  :after org
  :hook (org-mode . (lambda () (evil-org-mode)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; (use-package evil-surround
;;     :config
;;   (global-evil-surround-mode 1))

;; (use-package evil-org
;;     :hook (org-mode . (lambda () evil-org-mode))
;;     :config
;;     (require 'evil-org-agenda)
;;     (evil-org-agenda-set-keys))

;; (use-package evil-better-visual-line
;;   :ensure t
;;   :config
;;   (evil-better-visual-line-on))

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-s" . helm-occur)
         ("C-M-SPC" .  helm-imenu))
  :config
  (setq completion-styles '(flex))
  (setq helm-move-to-line-cycle-in-source nil)
  :init
  (helm-mode 1))

;; (use-package ivy
;;   :config
;;   (setopt ivy-use-virtual-buffers nil)
;;   :init
;;   (ivy-mode 1))

;; (use-package ivy-posframe
;;   :config
;;   (ivy-posframe-mode 1))

;; (use-package counsel
;;   :bind ("M-x" . #'counsel-M-x)
;;   :bind ("C-x C-f" . #'counsel-find-file)
;;   :init
;;   (counsel-mode 1))

(use-package swiper
    :bind (("C-s" . #'swiper)))

(use-package flycheck)

(use-package company
    :hook (after-init . global-company-mode)
    :bind ("M-/" . company-complete)
    :bind ("C-x C-/" . dabbrev-expand)
    :config
    (setq company-global-modes '(not org-mode))
    (setq company-idle-delay 0.3)
    (setq company-show-numbers t))

(use-package yasnippet
  :after (company)
  :hook (prog-mode . yas-minor-mode)
  :config
  (setq company-backends
        (append (or company-backends '()) '(company-yasnippet))))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

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
  (defun neo-path--insert-header-buttonized (path) nil)
  (setq neo-banner-message " "))

(use-package treemacs
  :bind ("M-0" . dougbeney/treemacs-smart-toggle))

; For some reason this only works if I define it outside of the use-package
(add-hook 'treemacs-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)))


(use-package avy
   :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-\"") 'avy-goto-char-2)
  (global-set-key (kbd "M-g g") 'avy-goto-line))

(use-package tab-bar
  :ensure nil
  :straight nil
  :bind (("M-1" . dougie-tab-select-or-create)
         ("M-2" . dougie-tab-select-or-create)
         ("M-3" . dougie-tab-select-or-create)
         ("M-4" . dougie-tab-select-or-create)
         ("M-5" . dougie-tab-select-or-create)
         ("M-6" . dougie-tab-select-or-create)
         ("M-7" . dougie-tab-select-or-create)
         ("M-8" . dougie-tab-select-or-create)
         ("M-9" . dougie-tab-select-or-create)
         ("M-t" . tab-bar-new-tab)
         ("M-[" . tab-previous)
         ("M-]" . tab-next)
         ("C-M-[" . dougie-tab-move-left)
         ("C-M-]" . dougie-tab-move-right))
  :init
  (defun dougie-tab-select-or-create (number)
    "Select tab NUMBER, appending tabs as needed."
    (interactive
     (let ((key (event-basic-type last-command-event)))
       (list (if (and (integerp key) (<= ?1 key ?9))
                 (- key ?0)
               (read-number "Tab number: " 1)))))
    (unless (> number 0)
      (user-error "Tab numbers start at 1"))
    (let ((count (length (funcall tab-bar-tabs-function))))
      (if (<= number count)
          (tab-bar-select-tab number)
        (tab-bar-new-tab-to -1))))

  (defun dougie-tab-move-left ()
    (interactive)
    (tab-move -1))

  (defun dougie-tab-move-right ()
    (interactive)
    (tab-move 1))

  :config
  (setq tab-bar-tab-hints t
        tab-bar-select-tab-modifiers nil
        tab-bar-show 1
        tab-bar-close-button-show nil
        tab-bar-new-tab-choice "*scratch*"
        tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-mode 1))

(with-eval-after-load 'eat
  (dolist (key '("M-1" "M-2" "M-3" "M-4" "M-5"
                 "M-6" "M-7" "M-8" "M-9"
                 "M-t" "M-[" "M-]" "C-M-[" "C-M-]"))
    (keymap-unset eat-semi-char-mode-map key)))

;; git related packages
(use-package magit
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package diff-hl
  :custom
  (diff-hl-disable-on-remote t)
  :hook
  ((prog-mode . turn-on-diff-hl-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh)))

(use-package expand-region
  :bind ("C-;" . er/expand-region))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this)))

;; Emacs terminal emulator
(use-package eat)

(use-package hideshow
    :ensure nil
    :bind (("C-," . hs-toggle-hiding)
           ("C-M-," . hs-show-all)
           ("M-," . hs-hide-all))
    :hook (prog-mode . hs-minor-mode)

    :config
    ;; Not sure if I need the below
    ;; Makes hideshow folding work with web-mode enabled
    ;; (add-to-list 'hs-special-modes-alist
    ;;               '(web-mode "{\\|<[^/>]*?" "}\\|</[^/>]*[^/]>" "<!--" web-mode-forward-sexp nil))
    )

(use-package project
    :ensure nil
    :config
    (global-set-key (kbd "M-m") project-prefix-map))
