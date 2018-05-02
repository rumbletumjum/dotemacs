    (defun org-refile-get-targets (&optional default-buffer)
      "Produce a table with refile targets."
      (let ((case-fold-search nil)
            ;; otherwise org confuses "TODO" as a kw and "Todo" as a word
            (entries (or org-refile-targets '((nil . (:level . 1)))))
            targets tgs files desc descre)
        (message "Getting targets...")
        (with-current-buffer (or default-buffer (current-buffer))
          (dolist (entry entries)
            (setq files (car entry) desc (cdr entry))
            (cond
             ((null files) (setq files (list (current-buffer))))
             ((eq files 'org-agenda-files)
              (setq files (org-agenda-files 'unrestricted)))
             ((and (symbolp files) (fboundp files))
              (setq files (funcall files)))
             ((and (symbolp files) (boundp files))
              (setq files (symbol-value files))))
            (when (stringp files) (setq files (list files)))
            (cond
             ((eq (car desc) :tag)
              (setq descre (concat "^\\*+[ \t]+.*?:" (regexp-quote (cdr desc)) ":")))
             ((eq (car desc) :todo)
              (setq descre (concat "^\\*+[ \t]+" (regexp-quote (cdr desc)) "[ \t]")))
             ((eq (car desc) :regexp)
              (setq descre (cdr desc)))
             ((eq (car desc) :level)
              (setq descre (concat "^\\*\\{" (number-to-string
                                              (if org-odd-levels-only
                                                  (1- (* 2 (cdr desc)))
                                                (cdr desc)))
                                   "\\}[ \t]")))
             ((eq (car desc) :maxlevel)
              (setq descre (concat "^\\*\\{1," (number-to-string
                                                (if org-odd-levels-only
                                                    (1- (* 2 (cdr desc)))
                                                  (cdr desc)))
                                   "\\}[ \t]")))
             (t (error "Bad refiling target description %s" desc)))
            (dolist (f files)
              (with-current-buffer (if (bufferp f) f (org-get-agenda-file-buffer f))
                (or
                 (setq tgs (org-refile-cache-get (buffer-file-name) descre))
                 (progn
                   (when (bufferp f)
                     (setq f (buffer-file-name (buffer-base-buffer f))))
                   (setq f (and f (expand-file-name f)))
                   (when (eq org-refile-use-outline-path 'file)
                     (push (list (file-name-nondirectory f) f nil nil) tgs))
                   (org-with-wide-buffer
                    (goto-char (point-min))
                    (setq org-outline-path-cache nil)
                    (while (re-search-forward descre nil t)
                      (beginning-of-line)
                      (let ((case-fold-search nil))
                        (looking-at org-complex-heading-regexp))
                      (let ((begin (point))
                            (heading (match-string-no-properties 4)))
                        (unless (or (and
                                     org-refile-target-verify-function
                                     (not
                                      (funcall org-refile-target-verify-function)))
                                    (not heading))
                          (let ((re (format org-complex-heading-regexp-format
                                            (regexp-quote heading)))
                                (target
                                 (if (not org-refile-use-outline-path) heading
                                   (concat
                                    (file-name-nondirectory (buffer-file-name (buffer-base-buffer)))
                                    " ✦ "
                                    (org-format-outline-path (org-get-outline-path t t) 1000 nil " ➜ ")
                                    ))))
                            (push (list target f re (org-refile-marker (point)))
                                  tgs)))
                        (when (= (point) begin)
                          ;; Verification function has not moved point.
                          (end-of-line)))))))
                (when org-refile-use-cache
                  (org-refile-cache-put tgs (buffer-file-name) descre))
                (setq targets (append tgs targets))))))
        (message "Getting targets...done")
        (delete-dups (nreverse targets))))
