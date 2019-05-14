(use-package company
  :ensure t
  :defer 5
  :config
  (setq company-idle-delay 0.1
        company-global-modes '(not org-mode)
        company-minimum-prefix-length 1)
  (add-hook 'after-init-hook 'global-company-mode))


(provide 'rtj-company)
