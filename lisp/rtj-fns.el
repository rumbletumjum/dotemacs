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

(defun defaults/shorten-yes-or-no ()
  "Don't ask `yes/no?', ask `y/n?'"
  (defalias 'yes-or-no-p 'y-or-n-p))

(defun open-next-line (arg)
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when #'newline-and-indent
    (indent-according-to-mode)))

(defun open-previous-line (arg)
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when #'newline-and-indent
    (indent-according-to-mode)))

(defun kill-default-buffer ()
  "Kill the currently active buffer -- set to C-x k so that users are not asked which buffer they want to kill."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun rtj/reset-theme ()
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes ))

(defun my-ibuffer-stale-p (&optional noconfirm)
  (frame-or-buffer-changed-p 'ibuffer-auto-buffers-changed))

(defun my-ibuffer-auto-revert-setup ()
  (setq-local buffer-stale-function 'my-ibuffer-stale-p)
  (setq-local auto-revert-verbose nil)
  (auto-revert-mode 1))

(defun windows/split-window-below-and-focus ()
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
	     (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun windows/split-window-right-and-focus ()
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
	     (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun window-thing (arg)
  (interactive "p")
  (cond
   ((= 1 (count-windows))
    (progn
      (split-window-right)
      (windmove-right)))
   ((= 2 (count-windows))
    (if (= arg 4)
        (progn
          (other-window 1)
          (delete-window))
      (other-window 1)))
   ((= 3 (count-windows))
    (if (= arg 4)
        (delete-other-windows))
    (ace-window 1))))

(defun arg-test (arg)
  (interactive "p")
  (message "%s" arg))

(provide 'rtj-fns)
