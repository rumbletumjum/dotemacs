(setq gc-cons-threshold (* 50 1000 1000)
      gc-cons-percentage 0.6)
(cond
 ((string-equal system-type "darwin")
  (set-face-attribute 'default nil :family "Roboto Mono" :weight 'regular  :height 120))
 ((string-equal system-type "gnu/linux")
  (set-face-attribute 'default nil :family "Roboto Mono" :weight 'regular  :height 100)))

(setq default-directory (getenv "HOME"))

(when (memq window-system '(mac ns))
  (progn
    (setq mac-command-modifier 'meta
          mac-option-modifier  'super)
    (add-to-list 'default-frame-alist
                 '(ns-transparent-titlebar . t))))

(when (eq window-system 'mac)
  (mac-auto-operator-composition-mode t))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

(defun rtj/package-init ()
  "Set package archives and initialize package system."
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
  			   ("org" . "https://orgmode.org/elpa/")))

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
    (require 'use-package))

  (use-package diminish
    :ensure t))

(rtj/package-init)
(rtj/bootstrap-use-package)

(setq use-package-verbose t
      use-package-always-ensure t
      use-package-verbose t)

(require 'rtj-themes)
(require 'rtj-proglang)
(require 'rtj-hydras)
(require 'rtj-ui)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq-default indent-tabs-mode nil)
(delete-selection-mode)

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist
      '((".*" . "~/.emacs.d/backups/")))

(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      backup-by-copying t)

(setq dired-dwim-target t)
(setq dired-ls-F-marks-symlinks t)
(setq dired-listing-switches "-lh")

(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq ring-bell-function 'ignore
      inhibit-splash-screen t
      initial-scratch-message nil)

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
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq ivy-count-format "(%d/%d) ")
    (setq ivy-enable-recursive-minibuffers t)
    (setq ivy-re-builders-alist
          '((swiper . ivy--regex-plus)
            (t . ivy--regex-fuzzy)))
    (setq ivy-initial-inputs-alist nil)
    (ivy-mode)))

(use-package flx
  :ensure t)

(use-package hydra
  :ensure t)

(use-package ivy-hydra
  :ensure t)

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-s") 'swiper))

(use-package counsel
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
  (counsel-mode))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :init
  (setq aw-scope 'frame)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
  (global-unset-key (kbd "C-x o")))

(use-package evil
  :ensure t
  :commands evil-mode
  :config
  (setq evil-move-cursor-back nil)

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(use-package magit
  :ensure t
  :commands magit-get-top-dir
  :bind (("C-c g" . magit-status)
         ("C-c C-g l" . magit-file-log)
         ("C-c f" . magit-grep)))

;; Lang

(use-package smartparens
  :ensure t
  :init
  (add-hook 'clojure-mode-hook #'smartparens-strict-mode)
  (add-hook 'racket-mode-hook #'smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (progn
    (setq sp-base-key-bindings 'paredit
          sp-autoskip-closing-pair 'always
          sp-hybrid-kill-entire-symbol nil)
    (sp-use-paredit-bindings)
    (sp-pair "{" nil
             :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair 'rust-mode "{" nil
                   :post-handlers '(("||\n[i]" "RET")
                                    ("| " "SPC")))))

(use-package rainbow-delimiters
  :ensure t
  :hook (((prog-mode cider-repl-mode) . rainbow-delimiters-mode)))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

(use-package ledger-mode
  :mode (("\\.ledger\\'" . ledger-mode)
         ("\\.beancount\\'" . ledger-mode)
         ("\\.journal\\'" . ledger-mode))
  :hook ((ledger-mode . (lambda ()
                          (setq-local tab-always-indent 'complete))))
  :config
  (setq ledger-highlight-xact-under-point nil
        ledger-post-amount-alignment-column 60))

(use-package pollen-mode
  :mode (("\\.pp'" . pollen-mode)
         ("\\.pm'" . pollen-mode)))

(use-package org
  :config
  (setq org-startup-indented t))

(use-package org-bullets
  :ensure t
  :commands (org-bullets-mode)
  :init
  (setq org-bullets-bullet-list
        '("○"))
  :hook (org-mode . org-bullets-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :init (setq markdown-header-scaling t
              markdown-header-scaling-values '(1.8 1.6 1.4 1.1 1.0 1.0))
    :config (setq markdown-asymmetric-header t))

(use-package racket-mode
  :commands racket-mode
  :bind (:map racket-mode-map
              ("[" . racket-smart-open-bracket)
              :map racket-repl-mode-map
              ("[" . racket-smart-open-bracket))
  :config
  (progn
    (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)
    (add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable))
  (setq racket-smart-open-bracket-enable t))

(setq explicit-shell-file-name "/usr/local/bin/zsh")


(progn
  (load-theme 'doom-solarized-light t)
  (setq doom-themes-enable-bold
        doom-themes-enable-italic))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config (setq doom-modeline-height 20))

(use-package undo-tree
  :diminish undo-tree-mode
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/tmp/undo"))
          undo-tree-auto-save-history t
          undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t)))

(global-set-key (kbd "M-[") 'previous-buffer)
(global-set-key (kbd "M-]") 'next-buffer)

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (progn
              (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)
              (setq gc-cons-threshold 16777216
                    gc-cons-percentage 0.1))))

(global-set-key (kbd "C-x 2") (lambda ()
                                (interactive)
                                (split-window-below)
                                (other-window 1)))

(global-set-key (kbd "C-x 3") (lambda ()
                                (interactive)
                                (split-window-right)
                                (other-window 1)))
