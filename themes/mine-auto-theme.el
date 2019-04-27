(autothemer-deftheme example-name "Autothemer example..."

  ;; Specify the color classes used by the theme
  ((((class color) (min-colors #xFFFFFF)))

   ;; Specify the color palette for each of the classes above.
   (foreground "#E1E2E7")
   (background "#1F2029")
   (plain-text "#000000")
   (characters "#1C00CF")
   (strings "#D3232E")
   (keywords "#D7008F")
   (comments "#45BB3E"))

    ;; specifications for Emacs faces.
  ((default (:foreground foreground :background background))
   (font-lock-keyword-face (:foreground keywords))
   (font-lock-type-face (:foreground "#1DA9A2"))
   (font-lock-variable-name-face (:foreground "#326D74"))
   (font-lock-string-face (:foreground strings))
   (font-lock-comment-face (:foreground comments)))

    ;; Forms after the face specifications are evaluated.
    ;; (palette vars can be used, read below for details.)
    )
