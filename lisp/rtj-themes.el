(defvar rtj/theme-hooks nil
  "((theme-id . function) ...)")

(defun rtj/switch-theme (theme-name)
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme-name t))

(defun rtj/add-theme-hook (theme-id hook-func)
  (add-to-list 'rtj/theme-hooks (cons theme-id hook-func)))

;; advise load-theme to disable other themes and look for any hooks defined for theme
;; hooks can be added by use-package call for theme to customize theme on the fly
;; see http://www.greghendershott.com/2017/02/emacs-themes.html
(defun rtj/load-theme-advice  (f theme-id &optional no-confirm no-enable &rest args)
  (unless no-enable
    (rtj/reset-theme))
  (prog1
      (apply f theme-id no-confirm no-enable args)
    (unless no-enable
      (pcase (assq theme-id rtj/theme-hooks)
        (`(,_ . ,f) (funcall f))))))

(advice-add 'load-theme
            :around #'rtj/load-theme-advice)

(use-package brutalist-theme :ensure t :defer t)
(use-package challenger-deep-theme :ensure t :defer t)
(use-package color-theme-sanityinc-tomorrow :ensure t :defer t)
(use-package creamsody-theme :ensure t :defer t)
(use-package darktooth-theme :ensure t :defer t)
(use-package dracula-theme :ensure t :defer t)
(use-package doneburn-theme :ensure t :defer t)
(use-package doom-themes :ensure t :defer t) ;; :custom-face ;; (org-level-1 ((t (:slant italic))))))
(use-package eziam-theme :ensure t :defer t)
(use-package farmhouse-theme :ensure t :defer t)
(use-package flatui-theme :ensure t :defer t)
(use-package gotham-theme :ensure t :defer t)
(use-package graphene-meta-theme :ensure t :defer t)
(use-package gruvbox-theme :ensure t :defer t)
(use-package kaolin-themes :ensure t :defer t)
(use-package leuven-theme :ensure t :defer t)
(use-package majapahit-theme :ensure t :defer t)
(use-package material-theme :ensure t :defer t)
(use-package minimal-theme :ensure t :defer t)
(use-package nord-theme :ensure t :defer t)
(use-package nordless-theme :ensure t :defer t)
(use-package plan9-theme :ensure t :defer t)
(use-package rebecca-theme :ensure t :defer t)
(use-package seoul256-theme :ensure t :defer t)
(use-package solarized-theme :ensure t :defer t)
(use-package spacemacs-theme :ensure t :defer t)
(use-package sourcerer-theme :ensure t :defer t)
(use-package subatomic-theme :ensure t :defer t)
(use-package sublime-themes :ensure t :defer t)
(use-package tao-theme :ensure t :defer t)
(use-package ujelly-theme :ensure t :defer t)
(use-package zenburn-theme :ensure t :defer t)
(use-package zerodark-theme :ensure t :defer t)

(provide 'rtj-themes)

