(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c S") 'windows/split-window-below-and-focus)
(global-set-key (kbd "C-c V") 'windows/split-window-right-and-focus)
(global-set-key (kbd "C-c w T") 'rtj/transpose-windows)
(global-set-key (kbd "C-c w t") 'rtj/window-split-toggle)

(global-set-key (kbd "C-h SPC") 'which-key-show-top-level)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x k") 'kill-default-buffer)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-i") 'hydra-window-two/body)
(global-set-key (kbd "M-o") 'rtj/windows)

(provide 'rtj-binds)
