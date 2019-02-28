(use-package scala-mode
  :ensure t
  :pin melpa
  :commands scala-mode)

(use-package ensime
  :ensure t
  :commands (ensime ensime-mode)
  :pin melpa-stable
  :config
  (setq ensime-startup-notification nil))

(use-package sbt-mode
  :ensure t
  :commands (sbt-mode)
  :pin melpa-stable)

(provide 'rtj-scala)
