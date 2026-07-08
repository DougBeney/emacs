;;;
;; general-purpose-langs-config.el
;; General purpose languages like Python, Ruby, or even C++
;;;
(provide 'general-purpose-langs-config)

(setq read-process-output-max (* 1024 1024))  ; 1MB
(setq gc-cons-threshold 100000000)

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package lisp-mode
  :ensure nil
  :straight nil
  :custom
  (lisp-indent-function 'common-lisp-indent-function))

(use-package pyvenv)

(use-package ruby-mode)

(use-package dart-mode
  :bind ("C-M-x" . #'flutter-hot-reload)
  :init
  ; env var for Flutter
  (setenv "CHROME_EXECUTABLE" "chromium")
  :config
  (use-package lsp-dart)
  (use-package flutter))

(defun flutter-enable-hot-reload-on-save-local ()
  (interactive)
  (flutter-run)
  (add-hook 'after-save-hook #'flutter-hot-reload nil t))

(defun flutter-enable-hot-reload-on-save ()
  (interactive)
  (add-hook 'after-save-hook #'flutter-hot-reload))

(defun flutter-disable-hot-reload-on-save ()
  (interactive)
  (disable-hook 'after-save-hook #'flutter-hot-reload))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))
