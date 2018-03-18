(defun rtj/window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows"
  (interactive)
  (if (> (length (window-list)) 2)
         (error "Can't toggle with more than 2 windows")
      (let ((func (if (window-full-height-p)
                      #'split-window-vertically
                    #'split-window-horizontally)))
        (delete-other-windows)
        (funcall func)
        (save-selected-window
          (other-window 1)
          (switch-to-buffer (other-buffer))))))

(defun rtj/transpose-windows ()
  "Transpose two windows if only 2 windows are visible"
  (interactive)
  (unless (= 2 (count-windows))
    (error "There are not 2 windows"))
  (let* ((windows (window-list))
         (window1 (car windows))
         (window2 (nth 1 windows))
         (window1-buffer (window-buffer window1))
         (window2-buffer (window-buffer window2)))
    (set-window-buffer window1 window2-buffer)
    (set-window-buffer window2 window1-buffer)))


(provide 'rtj-fns)
