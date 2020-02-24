(use-package rainbow-mode
  :ensure t
  :defer t)

;; (use-package powerline
;;   :ensure t
;;   :config
;;   (powerline-default-theme))


;;   (defun make-rect (color height width)
;;     "Create an XPM bitmap."
;;     (when window-system
;;       (propertize
;;        " " 'display
;;        (let ((data nil)
;;              (i 0))
;;          (setq data (make-list height (make-list width 1)))
;;          (pl/make-xpm "percent" color color (reverse data))))))


;;   (defun powerline-mode-icon ()
;;     (let ((icon (all-the-icons-icon-for-buffer)))
;;       (unless (symbolp icon) ;; This implies it's the major mode
;;         (format " %s"
;;                 (propertize icon
;;                             'help-echo (format "Major-mode: `%s`" major-mode)
;;                             'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))


;;   (setq-default mode-line-format 
;;                 '("%e"
;;                   (:eval
;;                    (let* ((active (powerline-selected-window-active))
;;                           (modified (buffer-modified-p))
;;                           (face1 (if active 'powerline-active1 'powerline-inactive1))
;;                           (face2 (if active 'powerline-active2 'powerline-inactive2))
;;                           (bar-color (cond ((and active modified) (face-foreground 'error))
;;                                            (active (face-background 'cursor))
;;                                            (t (face-background 'tooltip))))
;;                           (lhs (list
;;                                 (make-rect bar-color 30 3)
;;                                 (when modified
;;                                   (concat
;;                                    " "
;;                                    (all-the-icons-faicon "floppy-o"
;;                                                          :face (when active 'error)
;;                                                          :v-adjust -0.01)))
;;                                 " "
;;                                 (powerline-buffer-id)
;;                                 ))
;;                           (center (list
;;                                    " "
;;                                    (powerline-mode-icon)
;;                                    " "
;;                                    (powerline-major-mode)
;;                                    " "))
;;                           (rhs (list
;;                                 (format "%s" (eyebrowse--get 'current-slot))
;;                                 " | "
;;                                 (powerline-raw "%l:%c" 'mode-line 'r)
;;                                 " | "
;;                                 (powerline-raw "%6p" 'mode-line 'r)
;;                                 (powerline-hud 'highlight 'region 1)
;;                                 " "
;;                                 ))
;;                           )
;;                      (concat
;;                       (powerline-render lhs)
;;                       (powerline-fill-center face1 (/ (powerline-width center) 2.0))
;;                       (powerline-render center)
;;                       (powerline-fill face2 (powerline-width rhs))
;;                       (powerline-render rhs)))))))

;; (use-package spaceline-all-the-icons
;;   :ensure t
;;   :after spaceline
;;   :config (spaceline-all-the-icons-theme))

;; (defun custom-modeline-mode-icon ()
;;   (format " %s"
;;     (propertize icon
;;                 'help-echo (format "Major-mode: `%s`" major-mode)
;;                 'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))

;; (setq-default mode-line-format '("%e" (:eval 
;;   (concat
;;    (powerline-mode-icon)))))

;; (defun powerline-mode-icon ()
;;       (let ((icon (all-the-icons-icon-for-buffer)))
;;         (unless (symbolp icon) ;; This implies it's the major mode
;;           (format " %s"
;;                   (propertize icon
;;                               'help-echo (format "Major-mode: `%s`" major-mode)
;;                               'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))

(provide 'rtj-ui)
