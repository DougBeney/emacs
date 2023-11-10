;;;
;; indentation-and-whitespace-config.el
;;;

(provide 'indentation-and-whitespace-config)

(setq-default custom-tab-width 4)

(setq-default indent-tabs-mode t)
(setq-default tab-width custom-tab-width)
(setq-default py-indent-tabs-mode t)

(defun show-indentation ()
  (interactive)
  (setq whitespace-style '(face tabs tab-mark trailing))
  (setq whitespace-display-mappings
		'((tab-mark 9 [124 9] [92 9])))
  (whitespace-mode))

(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq python-indent-offset 4)
            (setq tab-width 4)))

;; (add-hook 'scss-mode-hook
;;          (lambda ()
;;            (setq indent-tabs-mode t)
;;            (setq tab-width 4)))

(add-hook 'php-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

(add-hook 'pug-mode-hook
		  (lambda ()
			(setq tab-width 2)
			(setq pug-tab-width 2)))

(defun dougbeney/use-spaces ()
  (setq indent-tabs-mode nil))

;; (add-hook 'emacs-lisp-mode 'dougbeney/use-spaces)
;; (add-hook 'lisp-mode 'dougbeney/use-spaces)
