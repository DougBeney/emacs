;; -*- lexical-binding: t; -*-

;;;
;; Theming-config.el
;;;

(provide 'theming-config)
(require 'helper-functions-config)

;; Theme Packs
(use-package doom-themes :defer t)
(use-package monokai-theme :defer t)
(use-package sublime-themes :defer t)

;; LOAD THE THEME
;; (add-hook 'elpaca-after-init-hook
;;           (lambda ()
;;             (load-theme 'doom-monokai-classic)))

(setq cursor-type 'bar)
(setq window-divider-default-places t)
(setq window-divider-default-bottom-width 5)
(setq window-divider-default-right-width 5)

(use-package all-the-icons :defer t)
(use-package nerd-icons) ; useful for custom modelines and such

(use-package doom-modeline
 :init
 (doom-modeline-mode 1))

(use-package rainbow-mode)

(defun set-tab-bar-colors (bg fg bg-inactive fg-inactive)
  ;; (set-tab-bar-colors "white" "black" "white" "gray")
  (when (display-graphic-p)
    (set-face-attribute
     'tab-bar nil
     :background bg-inactive
     :box nil)

    (set-face-attribute
     'tab-bar-tab nil
     :background bg
     :foreground fg
     :weight 'bold
     :height 100
     :width 'condensed
     :family "Roboto"
     :box `(:line-width (1 . 3) :color ,bg))

    (set-face-attribute
     'tab-bar-tab-group-current nil
     :inherit 'tab-bar-tab
     :box nil
     :weight 'bold)

    (set-face-attribute
     'tab-bar-tab-group-inactive nil
     :inherit '(shadow tab-bar-tab-inactive))

    (set-face-attribute
     'tab-bar-tab-inactive nil
     :background bg-inactive
     :foreground fg-inactive
     :weight 'bold
     :height 100
     :width 'condensed
     :family "Roboto"
     :box nil)

    (set-face-attribute
     'tab-bar-tab-ungrouped nil
     :inherit '(shadow tab-bar-tab-inactive))))

(defun dougbeney/tab-bar-padding (name tab _i)
  (let ((face (funcall tab-bar-tab-face-function tab)))
    (concat
     (propertize "   " 'face face)
     name
     (propertize "   " 'face face))))

(setq tab-bar-tab-name-format-functions
      '(tab-bar-tab-name-format-hints
        tab-bar-tab-name-format-close-button
        tab-bar-tab-name-format-face
        dougbeney/tab-bar-padding
        tab-bar-tab-name-format-mouse-face))

(setq tab-bar-separator "")
(setq tab-bar-auto-width t)

(setq dark-theme 'cyberpunk
      light-theme 'adwaita)

;; TODO: Refresh/update this
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

;; (load-theme dark-theme)

(defun set-tab-bar-colors-based-on-theme ()
  (interactive)
  (set-tab-bar-colors
   (face-attribute 'highlight :background)   ; Selected tab backgrounddes
   (face-attribute 'highlight :foreground)   ; Selected tab foreground
   (face-attribute 'default :background)   ; Inactive tab background
   (face-attribute 'default :foreground))  ; Inactive tab foreground
  )

(set-tab-bar-colors-based-on-theme)
(add-hook
 'server-after-make-frame-hook
 (lambda () (set-tab-bar-colors-based-on-theme)))

;; Not sure why I have this and if I need this
;(set-window-buffer nil (current-buffer))
