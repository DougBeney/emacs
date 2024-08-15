;;;
;; typography-config.el
;; Tweaks to the typography
;;;

(provide 'typography-config)

(require 'helper-functions-config)

;;; Set the default font
(dougbeney/set-font '(:family "FiraCode Nerd Font"
                              :height 100))

(setq-default line-spacing 0)
