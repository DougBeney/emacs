(provide 'space-leader-map-config)

(defvar-keymap dougbeney/bookmark-map
  :doc "Bookmark actions"
  "b" #'counsel-bookmark
  "m" #'bookmark-set
  "d" #'bookmark-delete
  "s" #'bookmark-save
  "l" #'bookmark-bmenu-list)

(defvar-keymap dougbeney/file-map
  :doc "File actions"
  "f" #'find-file
  "s" #'save-buffer)

(defvar-keymap dougbeney/window-map
  :doc "Window actions"
  "d" #'delete-window
  "l" #'split-window-right
  "j" #'split-window-below
  )

(defvar-keymap dougbeney/space-leader-map
  :doc "Awesome Menu"
  "b" (cons "Bookmarks" dougbeney/bookmark-map)
  "f" (cons "Files" dougbeney/file-map)
  "w" (cons "Windows" dougbeney/window-map)
  "s" (cons "Scratch buffer" (lambda () (interactive) (switch-to-buffer "*scratch*")))
  "o" (cons "Toggle olivetti" #'olivetti-mode))

;; space-leader-map for Vanilla Emacs
(keymap-global-set "M-SPC" dougbeney/space-leader-map)

;; space-leader-map for Evil
(when (require 'evil nil t)
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "SPC") dougbeney/space-leader-map)))
