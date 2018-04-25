(use-package powerline
  :ensure t
  :defer t)

(use-package spaceline-all-the-icons
  :ensure t
  :after spaceline
  :config (spaceline-all-the-icons-theme))

(defun custom-modeline-mode-icon ()
  (format " %s"
    (propertize icon
                'help-echo (format "Major-mode: `%s`" major-mode)
                'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))

;; (setq-default mode-line-format '("%e" (:eval 
;;   (concat
;;    (powerline-mode-icon)))))

(defun powerline-mode-icon ()
      (let ((icon (all-the-icons-icon-for-buffer)))
        (unless (symbolp icon) ;; This implies it's the major mode
          (format " %s"
                  (propertize icon
                              'help-echo (format "Major-mode: `%s`" major-mode)
                              'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))))


(provide 'rtj-ui)
