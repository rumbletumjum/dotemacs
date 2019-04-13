(use-package racket-mode
  :ensure t
  :commands racket-mode
  :config
  (progn
    (add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
    (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)))

(provide 'rtj-racket)

