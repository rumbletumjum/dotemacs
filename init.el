(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(set-face-attribute 'default nil :family "Hack" :height 120 :weight 'regular)

(setq default-directory (concat (getenv "HOME") "/"))

(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)
(setq inhibit-splash-screen t
      inhibit-startup-message t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq ring-bell-function 'ignore)

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super))

;;(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(defun custom/kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)


(use-package evil
  :init ;; tweak evil's configuration before loading it
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  :config ;; tweak evil after loading it
  (evil-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config (setq doom-modeline-height 20))

(use-package smartparens
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)))
;; (use-package rainbow-blocks
;;   :hook (emacs-lisp-mode . rainbow-blocks-mode))

;; (use-package rainbow-delimiters
;;   :hook (after-init . rainbow-delimiters-mode))

(straight-use-package 'rainbow-mode)

(use-package smex)
(use-package flx)
(use-package recentf)

(use-package ivy
  :bind (:map ivy-minibuffer-map
	      ("C-j" . ivy-immediate-done)
	      ("ret" . ivy-alt-done))
  :config (progn (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
					       (t . ivy--regex-fuzzy))
		       ivy-initial-inputs-alist nil)
		 (ivy-mode)))

(use-package counsel
  :bind (("C-x b" . counsel-buffer-or-recentf)
	 ("C-x C-f" . counsel-find-file)
	 ("C-c t" . counsel-load-theme)
	 ("C-x C-b" . counsel-ibuffer)
	 ("M-y" . counsel-yank-pop))
  :config (counsel-mode))

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package which-key
  :config 
	    (which-key-mode))

(use-package magit
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c f" . magit-grep)))

;; (use-package ace-window
;;   :bind (("M-o" . ace-window))
;;   :init (setq aw-scope 'frame)
;;   :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l)))

;; (use-package org
;;   :config (setq org-agenda-files '("~/Desktop/agenda/org")
;; 		org-startup-indented t
;; 		org-startup-folded 'content))

;; (use-package org-bullets
;;   :init (setq org-bullets-bullet-list (quote ("·")))
;;   :hook (org-mode . org-bullets-mode))

(use-package modus-vivendi-theme
  :config
  (load-theme 'modus-vivendi t))

(use-package esup
  :commands (esup))
