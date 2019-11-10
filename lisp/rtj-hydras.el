(defhydra hydra-zoom (:color amaranth)
  "zoom"
  ("+" text-scale-increase "in")
  ("=" text-scale-increase nil)
  ("-" text-scale-decrease "out")
  ("r" (text-scale-set 0) "reset")
  ("0" (text-scale-set 0) "foo" :exit t)
  ("q" nil "quit" :exit t))

(global-set-key (kbd "<f2>") 'hydra-zoom/body)

(defhydra hydra-themes (:hint nil :color pink)
  "
Themes

^Doom^        ^Solarized^
^^^^^^^-----------
_o_: one-dark _l_: light
"
  ("o" (load-theme 'doom-one t))
  ("l" (load-theme 'doom-solarized-light t))
  ("RET" nil "done" :color blue))

(global-set-key (kbd "C-c T") 'hydra-themes/body)

(provide 'rtj-hydras)
