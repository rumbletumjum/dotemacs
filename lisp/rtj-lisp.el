(use-package slime
  :ensure t
  :commands slime 
  :config
  ;; (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (setq slime-lisp-implementations
        '((sbcl ("sbcl"))
          (clisp ("clisp"))))
  (setq slime-contribs '(slime-fancy)))

(provide 'rtj-lisp)
