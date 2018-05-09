(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(provide 'rtj-haskell)
