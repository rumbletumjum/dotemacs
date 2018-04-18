(use-package slime
  :ensure t
  :config
  ;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-lisp-implementations
        '((sbcl ("sbcl"))
          (clisp ("clisp"))))
  (setq slime-contribs '(slime-fancy)))

(use-package racket-mode
  :ensure t)

(provide 'rtj-lisp)
