;;;
;; indentation-and-whitespace-config.el
;;;

(provide 'indentation-and-whitespace-config)

(setq-default custom-tab-width 4)
(setq-default indent-tabs-mode t)

(setq-default tab-width custom-tab-width)
(setq-default py-indent-tabs-mode indent-tabs-mode)


;; CUSTOM FUNCTIONS
(defun dougbeney/show-indentation ()
  (interactive)
  (setq whitespace-style '(face tabs tab-mark trailing))
  (setq whitespace-display-mappings
        '((tab-mark 9 [124 9] [92 9])))
  (whitespace-mode))

(defun dougbeney/python-indentation ()
	(setq indent-tabs-mode t)
	(setq python-indent-offset 4)
	(setq tab-width 4))

(defun dougbeney/use-spaces ()
  (setq indent-tabs-mode nil))

(defun dougbeney/use-tabs ()
  (setq indent-tabs-mode t))


;; HOOKS
(add-hook 'prog-mode-hook
          (lambda ()
            (dougbeney/show-indentation)
            (setq show-trailing-whitespace t)))

(add-hook 'python-mode-hook #'dougbeney/python-indentation)
(add-hook 'python-ts-mode-hook #'dougbeney/python-indentation)
(add-hook 'emacs-lisp-mode-hook 'dougbeney/use-spaces)
(add-hook 'lisp-mode-hook 'dougbeney/use-spaces)
