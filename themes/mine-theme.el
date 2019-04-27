(deftheme mine
  "Let's make a theme")

(let ((bg "#faf8f5")
      (fg "#111111")
      (uno1 "#2d2006")
      (uno2 "#896724")
      (uno3 "#b29762")
      (uno4 "#b6ad9a")
      (duo1 "#065289")
      (duo2 "#718ecd")
      (duo3 "#aeb3b7")
      (superwhite "#ffffff"))

  (custom-theme-set-faces
   'mine
   `(default ((t (:background ,bg :foreground ,uno1))))
   `(font-lock-builtin-face ((t (:background ,duo2 :foreground ,bg))))
   `(font-lock-keyword-face((t (:foreground ,duo1))))
   `(font-lock-type-face((t (:foreground ,duo2))))
   `(font-lock-function-name-face ((t (:foreground ,duo2))))
   `(tuareg-font-lock-governing-face ((t (:foreground ,duo1))))
   `(font-lock-variable-name-face((t (:background ,duo1 :foreground ,superwhite))))))


(provide-theme 'mine)
