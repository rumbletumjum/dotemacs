(use-package clojure-mode
  :ensure clojure-mode-extra-font-locking)

(use-package cider
  :ensure t
  :commands (cider cider-connect cider-jack-in))
  

(provide 'rtj-clojure)
