(defun rtj/switch-theme (theme-name)
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme-name t))

(use-package challenger-deep-theme
  :ensure t
  :defer t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package creamsody-theme
  :ensure t
  :defer t)

(use-package darktooth-theme
  :ensure t
  :defer t)

(use-package doom-themes
  :ensure t
  :defer t
  ;; :custom-face
  ;; (org-level-1 ((t (:slant italic)))))
  )

(use-package gruvbox-theme
  :ensure t
  :defer t)

(use-package kaolin-themes
  :ensure t
  :defer t)

(use-package leuven-theme
  :ensure t
  :defer t)

(use-package rebecca-theme
  :ensure t
  :defer t)

(use-package solarized-theme
  :ensure t
  :defer t)

(use-package zerodark-theme
  :ensure t
  :defer t)

(use-package graphene-meta-theme
  :ensure t
  :defer t)

(provide 'rtj-themes)

