;;;
;; indentation-and-whitespace-config.el
;;;

(provide 'indentation-and-whitespace-config)

(setq-default custom-tab-width 4)

(setq-default indent-tabs-mode t)
(setq-default show-trailing-whitespace t)
(setq-default tab-width custom-tab-width)

(setq-default py-indent-tabs-mode t)

(add-hook 'python-mode-hook
		  (lambda ()
			(setq indent-tabs-mode t)
			(setq python-indent 4)
			(setq tab-width 4)))
