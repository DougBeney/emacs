;;;
;; typography-config.el
;; Tweaks to the typography
;;;

(provide 'typography-config)

(require 'helper-functions-config)

;;; Set the default font
(dougbeney/set-font '(:family "Fira Code"
                              :height 110))

(setq-default line-spacing 0)
