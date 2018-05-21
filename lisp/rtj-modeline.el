(use-package powerline
  :ensure t
  :config
  (defun make-rect (color height width)
      "Create an XPM bitmap."
      (when window-system
        (propertize
         " " 'display
         (let ((data nil)
               (i 0))
           (setq data (make-list height (make-list width 1)))
           (pl/make-xpm "percent" color color (reverse data))))))
  
  ;; (setq-default mode-line-format
  ;;               '(" "
  ;;                 (:eval
  ;;                  (let* ((active (powerline-selected-window-active))
  ;;                         (face0 (if active 'powerline-active0 'powerline-inactive0))
  ;;                         (face1 (if active 'powerline-active1 'powerline-inactive1))
  ;;                         (face2 (if active 'powerline-active2 'powerline-inactive2))
  ;;                         (bar-color (cond ((and active modified) (face-foreground 'error))
  ;;                                            (active (face-background 'cursor))
  ;;                                            (t (face-background 'tooltip))))
  ;;                         (lhs (list 
  ;;                                                         (powerline-raw mode-line-modified face1)))
                          
  ;;                         (rhs (list (if (buffer-modified-p) (powerline-raw "Modified" face1 'r))))
  ;;                         (center (list (powerline-raw "%b" face0)
  ;;                                       " "
  ;;                                       (powerline-minor-modes face0)))
                          
  ;;                    (concat (powerline-render lhs)
  ;;                            (powerline-fill-center face0 (/ (powerline-width center) 2.0))
  ;;                            (powerline-render center)
  ;;                            (powerline-fill face0 (powerline-width rhs))
  ;;                            (powerline-render rhs)))))))



  (setq-default mode-line-format
                '(" "
                  (:eval
                   (let* (
                          (active (powerline-selected-window-active))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (lhs (list
                                ;; (make-rect (face-background 'powerline-active0) 30 3)
                                (powerline-raw mode-line-modified face0 'r)
                                (powerline-raw "%b" 'face2 'r)
                                (powerline-raw mode-line-position 'face0 'r)
                                ))
                          (center (list
                                   (powerline-raw mode-name 'face2 'r)
                                        ))
                          (rhs (list
                                (powerline-raw mode-line-misc-info))
                               ))
                     
                     (concat (powerline-render lhs)
                             (powerline-fill-center 'powerline-active0 (/ (powerline-width center) 2.0))
                             (powerline-render center)
                             (powerline-fill 'powerline-active0 (powerline-width rhs))
                             (powerline-render rhs)))))))


  ;; (setq-default mode-line-format
  ;;               '("-"
  ;;                 (:eval
  ;;                  (when (eql buffer-read-only t)
  ;;                    (propertize " READ ONLY " 'face
  ;;                        '(:background "color-88" :foreground "white" :weight bold))))
  ;;                 (:eval
  ;;                  (propertize " %b " 'face
  ;;                              (if (buffer-modified-p)
  ;;                                  '(:background "red" :foreground "#white" :weight bold)
  ;;                                '(:background "green" :foreground "#black" :weight bold))))
  ;;                 ))

  

