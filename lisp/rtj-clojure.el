(use-package clojure-mode
  :ensure clojure-mode-extra-font-locking
  :commands clojure-mode)

(use-package cider
  :ensure t
  :commands (cider-jack-in))

(provide 'rtj-clojure)
