(defhydra hydra-zoom (:color amaranth)
  "zoom"
  ("+" text-scale-increase "in")
  ("=" text-scale-increase nil)
  ("-" text-scale-decrease "out")
  ("r" (text-scale-set 0) "reset")
  ("0" (text-scale-set 0) "foo" :exit t)
  ("q" nil "quit" :exit t))

(global-set-key (kbd "<f2>") 'hydra-zoom/body)

(provide 'rtj-hydras)
