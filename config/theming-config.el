;;;
;; theming-config.el
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

(use-package all-the-icons :defer t)

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (use-package all-the-icons))

;; Terminal-related tweaks include:
;;   - Different font / font size
;;   - Different line-spacing
;;   - Auto-scroll on new output to terminal
(use-package term
  :commands ansi-term
  :hook (term-mode . (lambda ()
                       (dougbeney/set-font-local dougbeney-term-mode-face
                                                 (:family "Ubuntu Mono"
                                                          :height 90))
                       (setq-local line-spacing 0)
                       (setq-local mode-line-format nil)))
  :config
  ;; Auto-scroll terminal on new output
  (setq comint-scroll-to-bottom-on-input t))

(use-package all-the-icons-ivy
  :init
  (all-the-icons-ivy-setup))

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
