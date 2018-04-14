(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; (add-to-list 'default-frame-alist '(font . "Iosevka Term 14"))
(set-face-attribute 'default nil :font "Inconsolata 16")

(setq default-frame-alist '((ns-transparent-titlebar . t) (ns-appearance . 'nil)))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(defun rtj/package-init ()
  "Set package archives and initialize package system."
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
  			   ("melpa" . "https://melpa.org/packages/")
  			   ("org" . "https://orgmode.org/elpa/")))

  ;; (setq package-archives '(("gnu" . "~/elpa-mirror/gnu/")
  ;;       		   ("melpa" . "~/elpa-mirror/melpa/")
  ;;       		   ("org" . "~/elpa-mirror/org/")))

  (setq package-archive-priorities '(("org" . 3)
                                     ("melpa" . 2)
                                     ("gnu" . 1)))

  (setq package-enable-at-startup nil)
  (package-initialize))

(defun rtj/bootstrap-use-package ()
  "Ensure use-package is installed"
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (eval-when-compile
    (setq use-package-verbose 'debug)
    (require 'use-package))

  (use-package diminish
    :ensure t))

(rtj/package-init)
(setq use-package-verbose 'debug)
(rtj/bootstrap-use-package)

(require 'rtj-themes)
(require 'rtj-ui)

(defun defaults/shorten-yes-or-no ()
  "Don't ask `yes/no?', ask `y/n?'"
  (defalias 'yes-or-no-p 'y-or-n-p))

(defaults/shorten-yes-or-no)

(setq-default indent-tabs-mode nil)

(setq auto-save-default nil)
(setq make-backup-files nil)

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


(defun rtj/reset-theme ()
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes ))

;; (global-set-key (kbd "C-o") 'open-next-line)
;; (global-set-key (kbd "M-o") 'open-previous-line)
(global-set-key (kbd "C-h SPC") 'which-key-show-top-level)
(global-set-key (kbd "C-x k") 'kill-default-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)

;; (require 'dired+)
(setq dired-dwim-target t)
(setq dired-ls-F-marks-symlinks t)
(setq dired-listing-switches "-alh --group-directories-first")

;; (use-package ibuffer
;;   :commands ibuffer
;;   :bind ("C-x C-b" . ibuffer)
;;   :config (progn
;;             (use-package ibuf-ext)
;;             (add-hook 'ibuffer-mode-hook
;;                       (lambda ()
;;                         (local-set-key (kbd "r" 'ibuffer-update))))))

(defun my-ibuffer-stale-p (&optional noconfirm)
  (frame-or-buffer-changed-p 'ibuffer-auto-buffers-changed))

(defun my-ibuffer-auto-revert-setup ()
  (setq-local buffer-stale-function 'my-ibuffer-stale-p)
  (setq-local auto-revert-verbose nil)
  (auto-revert-mode 1))

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(setq ring-bell-function 'ignore)

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

;; (add-hook 'emacs-lisp-mode-hook #'xref-etags-mode)

(global-set-key (kbd "C-c S") 'windows/split-window-below-and-focus)
(global-set-key (kbd "C-c V") 'windows/split-window-right-and-focus)

(use-package recentf
  :config
  (setq recentf-max-saved-items 500
	recentf-max-menu-items 15
	recentf-auto-cleanup 'never)
  (recentf-mode))

(use-package smex
  :ensure t)

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode))

(use-package flx
  :ensure t)

;; (use-package hydra
;;   :ensure t)

(use-package ivy-hydra
  :ensure t)

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package counsel
  ;; :bind (("M-x" . counsel-M-x)
  ;;        ("C-h f" . counsel-describe-function)
  :bind (("C-c t" . counsel-load-theme)
         ("M-x" . counsel-M-x)
         ("C-h a" . counsel-apropos)
         ("C-x C-b". counsel-ibuffer)
         ("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :ensure t
  :diminish counsel-mode
  :config
  ;; (global-set-key (kbd "C-h v") 'counsel-describe-variable)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  (counsel-mode))


(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char-timer)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :config
  (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 3.0)
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (global-unset-key (kbd "C-x o")))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(require 'rtj-clojure)
(require 'rtj-fns)
(require 'rtj-lisp)
(require 'rtj-racket)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings))

(global-set-key (kbd "C-c w t") 'rtj/window-split-toggle)
(global-set-key (kbd "C-c w T") 'rtj/transpose-windows)
(global-set-key (kbd "C-x g") 'magit-status)

(use-package rainbow-delimiters
  :ensure t
  :hook (((prog-mode cider-repl-mode) . rainbow-delimiters-mode)))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

(use-package osx-trash
  :ensure t
  :config
  (when (eq system-type 'darwin)
    (osx-trash-setup))
  (setq delete-by-moving-to-trash t))

(use-package clojure-mode
  :ensure t)

(use-package expand-region
  :bind (("C-=" . er/expand-region))
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :config
  (setq golden-ratio-auto-scale t)
  (golden-ratio-mode))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init
  (setq org-bullets-bullet-list
        '("◉" "◎" "○" "●" "◇"))
  :hook (org-mode . org-bullets-mode))

(setq org-capture-templates
      '(("l" "A link, for reading later." entry
         (file+headline "notes.org" "Reading List")
         "** %:description\n%:link\n%u"
         :empty-lines 1)))

(require 'org-protocol)

;; (use-package spaceline
;;   :ensure t
;;   :config
;;   (require 'spaceline-config)
;;   (spaceline-emacs-theme))

;; (load-theme 'doom-solarized-light t)

(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window ()
   "window"
   ("h" windmove-left "left")
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("a" ace-window)))

(defhydra hydra-window-two (:color blue)
  "window2"
  ("h" windmove-left "left")
  ("j" windmove-down "down")
  ("k" windmove-up "up")
  ("l" windmove-right "right"))

(global-set-key (kbd "M-i") 'hydra-window-two/body)

(use-package pdf-tools
  :pin manual ;; manually update
  :defer t
  :config
  ;; initialise
  (pdf-tools-install)
  ;; open pdfs scaled to fit page
  (setq-default pdf-view-display-size 'fit-page)
  ;; automatically annotate highlights
  (setq pdf-annot-activate-created-annotations t)
  ;; use normal isearch
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-asymmetric-header t)
  (setq markdown-header-scaling t))

(setq explicit-shell-file-name "/usr/local/bin/zsh")

(use-package ibuffer
  :commands ibuffer
  :bind ("C-x C-b" . ibuffer)
  :config (progn
            (use-package ibuf-ext)
            ;; ibuffer, I like my buffers to be grouped
            (add-hook 'ibuffer-mode-hook
                      (lambda ()
                        (my-ibuffer-auto-revert-setup)
                        (local-set-key (kbd "r") 'ibuffer-update)
                        (ibuffer-switch-to-saved-filter-groups
                         "default")))
            (setq ibuffer-saved-filter-groups
                  (quote (("default"
                           ("dired" (mode . dired-mode))
                           ("org" (mode . org-mode))
                           ("magit" (name . "\*magit"))
                           ("emacs" (or
                                     (name . "^\\*scratch\\*$")
                                     (name . "^\\*Messages\\*$")))))))))

(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
                                    
;; (use-package helm
;;   :ensure t
;;   :bind ("M-x" . 'helm-M-x)
;;   :config
;;   (setq helm-M-x-fuzzy-match t
;;         helm-split-window-in-side-p t))

;; (defun window-thing (arg)
;;   (interactive "p")
;;   (cond
;;    ((= 1 (count-windows))
;;     (windows/split-window-right-and-focus))
;;    ((= 2 (count-windows))
;;     (if ))
;;    (t (ace-window 1))))

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

(global-set-key (kbd "M-o") 'window-thing)
(put 'dired-find-alternate-file 'disabled nil)
