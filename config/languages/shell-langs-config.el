(provide 'shell-langs-config)

(use-package fish-mode)

(setq tramp-default-remote-shell "/bin/sh")

(with-eval-after-load 'tramp
  (setq tramp-remote-shell "/bin/sh")
  (setq tramp-remote-shell-args '("-c")))
