(defun my/org-refile-to-journal ()
  "Refile an entry to journal file's date-tree"
  (interactive)
  (require 'org-datetree)
  (let ((journal "/data/life-in-plain-text/journal.org")
        post-date)
    (setq post-date (or (org-entry-get (point) "TIMESTAMP_IA")
                        (org-entry-get (point) "TIMESTAMP")))
    (setq post-date (nthcdr 3 (parse-time-string post-date)))
    (setq post-date (list (cadr post-date) 
                          (car post-date) 
                          (caddr post-date)))
    (org-cut-subtree)
    (with-current-buffer (or (find-buffer-visiting journal)
                             (find-file-noselect file))
      (save-excursion
        (org-datetree-file-entry-under (current-kill 0) post-date)
        (bookmark-set "org-refile-last-stored")))
    (message "Refiled to %s" journal)))

(defun my/org-agenda-refile-to-journal ()
  "Refile the item at point to journal."
  (interactive)
  (let* ((marker (or (org-get-at-bol 'org-hd-marker)
                     (org-agenda-error)))
         (buffer (marker-buffer marker))
         (pos (marker-position marker)))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char marker)
          (org-remove-subtree-entries-from-agenda)
          (my/org-refile-to-journal)))))
  (org-agenda-redo))

(org-defkey org-agenda-mode-map (kbd "C-c C-S-w") 'my/org-agenda-refile-to-journal)
(org-defkey org-mode-map (kbd "C-c C-S-w") 'my/org-refile-to-journal)