;;;
;; Theming-config.el
;;;

(provide 'theming-config)

(require 'helper-functions-config)

(setq cursor-type 'bar)
(setq window-divider-default-places t)
(setq window-divider-default-bottom-width 5)
(setq window-divider-default-right-width 5)

;; Themes
;; (use-package cyberpunk-theme :defer t)
;; (use-package dracula-theme :defer t)
;; (use-package atom-one-dark-theme :defer t)
;; (use-package doom-themes :defer t)
;; (use-package gruvbox-theme :defer t)
;; (use-package monokai-theme :defer t)
;; (use-package sublime-themes :defer t)
;; (use-package catppuccin-theme
;;   :vc (:url "https://github.com/catppuccin/emacs"
;; 	    :rev :newest)
;;   :init
;;   (setq catppuccin-flavor 'mocha)
;;   :config
;;   (load-theme 'catppuccin)
;;   (set-tab-bar-colors-based-on-theme))

;; (use-package modus-themes
;;     :config
;;   (modus-themes-load-theme 'modus-vivendi))

(use-package modus-flexoki
    :straight (:type git
               :host github
               :repo "dpassen/modus-flexoki")
    :demand t
    :config
    (load-theme 'modus-flexoki-dark :no-confirm))


(use-package all-the-icons :defer t)
(use-package nerd-icons) ; useful for custom modelines and such

;; (use-package doom-modeline
;;  :init
;;  (doom-modeline-mode 1)
;;  :config
;;  (setq doom-modeline-hud t)
;;  (setq doom-modeline-buffer-encoding nil))

(use-package mood-line
  :config
  (mood-line-mode)
  (set-face-attribute 'header-line nil
                      :box `(:line-width (3 . 3)
                                         :color ,(face-background 'header-line)))
  :custom
  (mood-line-glyph-alist mood-line-glyphs-fira-code)
  (mood-line-format mood-line-format-default-extended)
)

(use-package rainbow-mode)

(defun set-tab-bar-colors (bg fg bg_inactive fg_inactive)
  ;; (set-tab-bar-colors "white" "black" "white" "gray")
  (when (display-graphic-p)
	(custom-set-faces
	 `(tab-bar ((t (:box nil))))
	 `(tab-bar-tab ((t (:background ,bg :foreground ,fg :weight bold :height 99 :width condensed :family "Roboto" :box (:line-width (10 . 10) :color ,bg)))))
	 `(tab-bar-tab-group-current ((t (:inherit tab-bar-tab :box nil :weight bold))))
	 `(tab-bar-tab-group-inactive ((t (:inherit (shadow tab-bar-tab-inactive)))))
	 `(tab-bar-tab-inactive ((t (:inherit tab-line-tab :background ,bg_inactive :foreground ,fg_inactive :weight bold :height 99 :width condensed :family "Roboto"))))
	 `(tab-bar-tab-ungrouped ((t (:inherit (shadow tab-bar-tab-inactive))))))))

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
