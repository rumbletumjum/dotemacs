(defun rtj/windows (arg)
  (interactive "p")
  (cl-case (count-windows)
    (1 (rtj--windows-fn1 arg))
    (2 (rtj--windows-fn2 arg))
    (t (ace-window arg))))

(defun rtj--windows-fn1 (arg)
  (cl-case arg
    (4 (rtj--windows-split-window-below-and-focus))
    (t (rtj--windows-split-window-right-and-focus))))

(defun rtj--windows-fn2 (arg)
  (if (eq 16 arg)
      (delete-other-windows)
    (other-window 1)))

(defun rtj--windows-split-window-below-and-focus ()
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
	     (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun rtj--windows-split-window-right-and-focus ()
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
	     (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun rtj/arg-test (arg)
  (interactive "p")
  (message "%s" arg))

(provide 'rtj-windows)
      
