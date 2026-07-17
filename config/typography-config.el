;;;
;; typography-config.el
;; Tweaks to the typography
;;;

(provide 'typography-config)

;; Set font & size here...
(setq dougbeney/typography '(:font "JetBrainsMono Nerd Font"
                             :size 110))

(defun dougbeney/set-font (options &optional face)
  (let ((family (plist-get options :family))
        (weight (plist-get options :weight))
        (width (plist-get options :width))
        (height (plist-get options :height))
        (the-face (if face face 'default)))
    (set-face-attribute the-face nil
                        :family (if family family (face-attribute the-face :family))
                        :height (if height height (face-attribute the-face :height))
                        :weight (if weight weight (face-attribute the-face :weight))
                        :width (if width width (face-attribute the-face :width)))))

(defmacro dougbeney/set-font-local (new-face-name options &optional new-face-description)
  (let ((new-face-name (gensym new-face-name))
        (new-face-description
         (if new-face-description
             new-face-description
           "Custom face created by user")))
    `(progn
       (defface ,new-face-name
         '((t ,@options))
         ,new-face-description)
       (buffer-face-set ',new-face-name))))

;;; Set the default font
(dougbeney/set-font (list :family (plist-get dougbeney/typography :font)
                          :height (plist-get dougbeney/typography :size)
                          :weight 'normal))

(setq-default line-spacing 0)
(setq-default truncate-lines t)
