;;;
;; Theming-config.el
;;;

(provide 'theming-config)

(require 'helper-functions-config)

(setq window-divider-default-places t)
(setq window-divider-default-bottom-width 5)
(setq window-divider-default-right-width 5)

;; Themes
(use-package cyberpunk-theme :defer t)
(use-package dracula-theme :defer t)
(use-package atom-one-dark-theme :defer t)
(use-package doom-themes :defer t)
;; (use-package gruvbox-theme :defer t)
(use-package monokai-theme :defer t)
(use-package sublime-themes :defer t)

(use-package all-the-icons :defer t)

(use-package doom-modeline
 :init
 (doom-modeline-mode 1)
 :config
 (setq doom-modeline-hud t)
 (setq doom-modeline-buffer-encoding nil))

(defun dougie-modeline ()
  (interactive)
  (let ((new-format
		 (list
		  (format "%s "
				  (propertize (all-the-icons-icon-for-buffer)
							  'face `(:height 1.5 :family ,(all-the-icons-icon-family-for-buffer))))
		  mode-line-buffer-identification
		  mode-line-end-spaces
		  " "
		  (if (fboundp 'eyebrowse--get)
			  (number-to-string (eyebrowse--get 'current-slot))
			"")
		  " %e"
		  )))
	(setq-default mode-line-format new-format)
	(setq mode-line-format new-format)) t)

;; Activate my modeline
;; (dougie-modeline)
;; (add-hook 'post-command-hook #'dougie-modeline)
;; (add-hook 'window-configuration-change-hook #'dougie-modeline)

(use-package all-the-icons)

;; Terminal-related tweaks include:
;;   - Different font / font size
;;   - Different line-spacing
;;   - Auto-scroll on new output to terminal
(use-package term
  :commands ansi-term
  ;; :hook (term-mode . (lambda ()
  ;;                      (dougbeney/set-font-local dougbeney-term-mode-face
  ;;                                                (:family "Ubuntu Mono"
  ;;                                                         :height 90))
  ;;                      (setq-local line-spacing 0)
  ;;                      (setq-local mode-line-format nil)))
  :config
  ;; Auto-scroll terminal on new output
  (setq comint-scroll-to-bottom-on-input t))

(use-package all-the-icons-ivy
  :init
  (all-the-icons-ivy-setup))

(use-package rainbow-mode)

(defun set-tab-bar-colors (bg fg bg_inactive fg_inactive)
  (custom-set-faces
   `(tab-bar ((t (:box nil))))
   `(tab-bar-tab ((t (:background ,bg :foreground ,fg :box (:line-width (10 . 10) :color ,bg :style flat-button) :weight bold :height 99 :width condensed :family "Roboto"))))
   `(tab-bar-tab-group-current ((t (:inherit tab-bar-tab :box nil :weight bold))))
   `(tab-bar-tab-group-inactive ((t (:inherit (shadow tab-bar-tab-inactive)))))
   `(tab-bar-tab-inactive ((t (:inherit tab-line-tab :background ,bg_inactive :foreground ,fg_inactive :box (:line-width (10 . 10) :color ,bg_inactive :style flat-button) :weight bold :height 99 :width condensed :family "Roboto"))))
   `(tab-bar-tab-ungrouped ((t (:inherit (shadow tab-bar-tab-inactive)))))))

;; Example of using theme colors
;; (set-tab-bar-colors
;;  (face-attribute 'highlight :background)   ; Selected tab backgrounddes
;;  (face-attribute 'highlight :foreground)   ; Selected tab foreground
;;  (face-attribute 'default :background)   ; Inactive tab background
;;  (face-attribute 'default :foreground))  ; Inactive tab foreground

(set-tab-bar-colors "white" "black" "white" "gray")

;; Fancy pants stuff for auto theme-switching

;; (setq dark-theme 'cyberpunk
;;       light-theme 'adwaita)

;; (load-theme dark-theme)
;; (load-theme light-theme t t)

;; (setq dark-theme-activated t
;;       light-theme-activated nil)

;; (add-hook 'pre-command-hook
;;           (lambda ()
;;             (dougbeney/set-theme-based-on-mode "pre command hook")))

;; (add-hook 'after-change-major-mode-hook
;;           (lambda ()
;;             (dougbeney/set-theme-based-on-mode "after change major mode hook")))
