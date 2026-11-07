;; -*- lexical-binding: t; -*-

(use-package which-key
  :ensure t
  :config
  (which-key-setup-side-window-bottom)   ; multicolumn panel at the bottom
  (setq which-key-idle-delay 0
        which-key-idle-secondary-delay 0.01
        which-key-sort-order 'which-key-key-order-alpha
        which-key-max-description-length 32
        which-key-max-display-columns nil   ; as many columns as fit
        which-key-add-column-padding 1
        which-key-separator " → "
        which-key-prefix-prefix "+"         ; "+Files", "+Windows" for submenus
        which-key-side-window-max-height 0.25)
  (which-key-mode 1))

(use-package general
  :ensure t
  :config

  ;; Vanilla Emacs Binding
  (general-create-definer dougbeney/leader-vanilla
    :keymaps 'override
    :prefix "M-SPC")

  ;; Evil Binding
  (general-create-definer dougbeney/leader
    :states '(normal visual motion insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (defconst dougbeney/leader-bindings
    '(;; Bookmarks
      "b"  (:ignore t :wk "Bookmarks")
      "bb" (helm-bookmarks :wk "list (helm)")
      "bm" (bookmark-set :wk "set")
      "bd" (bookmark-delete :wk "delete")
      "bs" (bookmark-save :wk "save")
      "bl" (bookmark-bmenu-list :wk "menu")

      ;; Files
      "f"  (:ignore t :wk "Files")
      "ff" (find-file :wk "find file")
      "fs" (save-buffer :wk "save")

      ;; Windows
      "w"  (:ignore t :wk "Windows")
      "wd" (delete-window :wk "delete")
      "wl" (split-window-right :wk "split right")
      "wj" (split-window-below :wk "split below")

      ;; Gptel
      "g"  (:ignore t :wk "Gptel")
      "gg" (gptel-send :wk "send")
      "gm" (gptel-menu :wk "menu")
      "gr" (gptel-rewrite :wk "rewrite")

      ;; Emoji
      "e"  (:ignore t :wk "Emoji")
      "ee" (emoji-insert :wk "insert")
      "es" (emoji-search :wk "search")
      "er" (emoji-recent :wk "recent")

      ;; Single keys
      "s"  (scratch-buffer :wk "Scratch buffer")
      "o"  (olivetti-mode :wk "Toggle olivetti")))

  ;; The definers are macros that evaluate their args, so quote each
  ;; binding spec and splice the list in with eval.
  (let ((args (mapcar (lambda (x) (if (consp x) (list 'quote x) x))
                      dougbeney/leader-bindings)))
    (eval `(dougbeney/leader ,@args) t)
    (eval `(dougbeney/leader-vanilla ,@args) t)))

(provide 'space-leader-map-config)
