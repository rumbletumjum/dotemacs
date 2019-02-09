(use-package scala-mode
  :ensure t
  :pin melpa
  :commands scala-mode)

(use-package ensime
  :ensure t
  :pin melpa-stable
  :hook (scala-mode . ensime)
  :config
  (setq ensime-startup-notification nil))

(use-package sbt-mode
  :ensure t
  :pin melpa-stable)

(provide 'rtj-scala)
