(use-package haskell-mode
  :ensure t
  :commands haskell-mode)

(use-package intero
  :ensure t
  :defer t
  :hook (haskell-mode . intero-mode))

(use-package hindent
  :ensure t)
;; (add-hook 'haskell-mode-hook #'hindent-mode)


(provide 'rtj-haskell)
