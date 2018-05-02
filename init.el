(defun package-init ()
  "Set package archives and initialize package system"
  (setq package-archives '(("org" . "//?/h:/spacemacs-0.200.13/elpa-mirror-master/org/")
                           ("gnu" . "//?/h:/spacemacs-0.200.13/elpa-mirror-master/gnu/")
                           ("melpa" . "//?/h:/spacemacs-0.200.13/elpa-mirror-master/melpa/")))

  (setq package-enable-at-startup nil)
  (package-initialize))

(defun bootstrap-use-package ()
  "Ensure use-package is installed"
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile
    (require 'use-package)))

(package-init)
(bootstrap-use-package)

;; (add-to-list 'default-frame-alist '(font . "Fira Code 10"))
;;(set-face-attribute 'default nil :family "Pragsevka" :height 90)

(set-face-attribute 'default nil :family "Fira Code" :height 100)
;; (set-face-attribute 'default nil :font "Pragsevka 12")
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(mapcar #'disable-theme custom-enabled-themes)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(defun defaults/clean-interface ()
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode)
      (funcall mode -1)))
  (setq inhibit-startup-screen t))

(defun defaults/shorten-yes-or-no ()
  "Don't ask `yes/no?', ask `y/n?'"
  (defalias 'yes-or-no-p 'y-or-n-p))

(defaults/shorten-yes-or-no)
(defaults/clean-interface)

(defun utils/open-init-file ()
  "Shortcut to open init.el"
  (interactive)
  (window-fns/split-window-right-and-focus)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key (kbd "C-c I") 'utils/open-init-file)


(add-hook 'before-save-hook 'time-stamp)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default indent-tabs-mode nil)
(delete-selection-mode)

(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist
      `((".*" . "~/.emacs.d/backups")))

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      backup-by-copying t)

(defun window-fns/split-window-below-and-focus()
  (interactive)
  (split-window-below)
  (windmove-down)
  (when (and (boundp 'golden-ratio-mode)
     (symbol-value golden-ratio-mode))
    (golden-ratio)))

(defun window-fns/split-window-right-and-focus()
  (interactive)
  (split-window-right)
  (windmove-right)
  (when (and (boundp 'golden-ratio-mode)
     (symbol-value golden-ratio-mode))
    (golden-ratio)))

(global-set-key (kbd "C-c C-l") 'org-insert-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b n") 'next-buffer)
(global-set-key (kbd "C-c b p") 'previous-buffer)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c w F") 'delete-frame)
(global-set-key (kbd "C-c w f") 'make-frame)
(global-set-key (kbd "C-c w s") 'window-fns/split-window-below-and-focus)
(global-set-key (kbd "C-c w v") 'window-fns/split-window-right-and-focus)
(global-set-key (kbd "C-h SPC") 'which-key-show-top-level)
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
;; (global-set-key (kbd "M-o") 'other-window)

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :config
  (setq aw-dispatch-always t))

(defun keybinds/M-o-window-fn ()
  (interactive)
  (if (= 1 (count-windows))
      (message "1 Window")
    (message "%d Windows" (count-windows))))

(setq sentence-end-double-space nil)

(use-package diminish
  :ensure t)

(use-package smex
  :ensure t)

(use-package flx
  :ensure t)

(use-package org
  :ensure t
  :bind (("C-c c" . org-capture))
  :config
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-files '("z:/Desktop/agenda/"))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 4)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-log-redeadline (quote time))
  (setq org-log-done (quote time))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c@)"))
        org-log-done 'time)
  (setq org-capture-templates
        '(("n" "note" entry (file "z:/Desktop/org/notes.org")
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("l" "Log" entry (file+olp+datetree "z:/Desktop/agenda/log.org")
           "* %? \n%U" :clock-in t :clock-keep t)
          ("t" "Task" entry (file+olp+datetree "z:/Desktop/agenda/today.org")
           "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")))
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
        (delete-dups (nreverse targets)))))

;; (require 'org)
  ;; (setq org-enforce-todo-dependencies t)
  ;; (setq org-agenda-files '("z:/Desktop/agenda/"))
  ;; (setq org-refile-targets '((org-agenda-files :maxlevel . 4)))
  ;; (setq org-refile-use-outline-path 'file)
  ;; (setq org-outline-path-complete-in-steps nil)
  ;; (setq org-todo-keywords
  ;;       '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELLED(c@)"))
  ;;       org-log-done 'time)
  ;; (setq org-capture-templates
  ;;       '(("n" "note" entry (file "z:/Desktop/org/notes.org")
  ;;          "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
  ;;         ("l" "Log" entry (file+olp+datetree "z:/Desktop/org/log.org")
  ;;          "* %? \n%U" :clock-in t :clock-keep t)
  ;;         ("t" "Task" entry (file+olp+datetree "z:/Desktop/agenda/today.org")
  ;;          "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n"))

(use-package org-bullets
  :ensure t
  :config
  (setq org-bullets-bullet-list "#")
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode))))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode))

(use-package ace-window
  :ensure t)

(use-package avy
  :ensure t)

(use-package hydra
  :ensure t)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper))
  :config
  (setq ivy-count-format "(%d/%d) "))

(use-package counsel
  :ensure t
  :bind ("C-c t" . counsel-load-theme))

(use-package anzu
  :ensure t
  :diminish global-anzu-mode
  :config
  (global-anzu-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :defer t)

(use-package challenger-deep-theme
  :ensure t
  :defer t)

(use-package dakrone-theme
  :ensure t
  :defer t)

(use-package darktooth-theme
  :ensure t
  :defer t)

(use-package doom-themes
  :ensure t
  :defer t)

(use-package eziam-theme
  :ensure t
  :defer t)

(use-package gruvbox-theme
  :ensure t
  :defer t)

(use-package solarized-theme
  :ensure t
  :defer t)

(use-package sublime-themes
  :ensure t
  :defer t)

(use-package tao-theme
  :ensure t
  :defer t)

(use-package zerodark-theme
  :ensure t
  :defer t)

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-asymmetric-header t)
  (add-hook 'markdown-mode-hook 'turn-on-auto-fill)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (set-fill-column 90))))

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
  (golden-ratio-mode)
  (add-to-list 'golden-ratio-extra-commands 'ace-window))

(use-package expand-region
  :ensure t
  :bind(("C-=" . 'er/expand-region)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)))

;; (setq lisp-ls-use-insert-directory-program t)

;; (setq insert-directory-program "i:/sw/external/gnuwin32-1.0/bin/ls.exe")
(setq lisp-ls-ignore-case t)
(setq lisp-ls-dirs-first t)
(setq dired-listing-switches "-lh")
(put 'dired-find-alternate-file 'disabled nil)

(add-to-list 'auto-mode-alist '("\\.sp\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.tab\\'" . sql-mode))

(use-package ibuffer
  :commands ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config
  (progn
  (use-package ibuf-ext)
  ;; ibuffer, I like my buffers to be grouped
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (local-set-key (kbd "r") 'ibuffer-update)
              (ibuffer-switch-to-saved-filter-groups
               "default")))
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("dired" (mode . dired-mode))
                 ("org" (mode . org-mode))
                 ("java" (name . "^\\*java\\$"))
                 ("perl" (mode . perl-mode))
                 ("sql" (mode . sql-mode))
                 ("planner" (or
                             (name . "^\\*Calendar\\*$")
                             (name . "^diary$")
                             (mode . muse-mode)))
                 ("emacs" (or
                           (name . "^\\*scratch\\*$")
                           (name . "^\\*Messages\\*$")))))))))

(load-theme 'doom-citylights t)

(global-set-key (kbd "<f2>")
                (defhydra hydra-zoom (:color amaranth)
                  "zoom"
                  ("+" text-scale-increase "increase")
                  ("0" (text-scale-set 0) "reset")
                  ("-" text-scale-decrease "decrease")
                  ("=" text-scale-increase nil)
                  ("q" nil "quit" :color blue)))

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
