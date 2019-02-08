(use-package scala-mode
  :ensure t
  :pin melpa
  :commands scala-mode)

(use-package ensime
  :ensure t
  :pin melpa-stable
  :hook (scala-mode . ensime))

(use-package sbt-mode
  :ensure t
  :pin melpa)

(provide 'rtj-scala)
