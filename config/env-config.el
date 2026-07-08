;;;
;; env-config.el
;; Different features for setting up the environment of emacs. Ex. Tweaking PATH
;;;

(provide 'env-config)

(setq exec-path (append exec-path (split-string (shell-command-to-string "echo $PATH"))))
(setenv "DISPLAY" ":0")
(setenv "WAYLAND_DISPLAY" "wayland-1")
