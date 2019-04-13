(use-package go-mode
  :ensure t
  :commands go-mode
  :hook ((go-mode . (lambda ()
                      (setq tab-width 4)))))

(provide 'rtj-go)
