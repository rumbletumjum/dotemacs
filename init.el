(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; (add-to-list 'default-frame-alist '(font . "Iosevka Term 14"))
(set-face-attribute 'default nil :font "Fira Code 12")

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
    (require 'use-package)
    (setq use-package-verbose t))


  (use-package diminish
    :ensure t))

(rtj/package-init)
;; (setq use-package-verbose t)
(rtj/bootstrap-use-package)

(require 'rtj-fns)
(require 'rtj-themes)
(require 'rtj-ui)


(defaults/shorten-yes-or-no)

(setq-default indent-tabs-mode nil)

(delete-selection-mode)

(setq auto-save-default nil)
(setq make-backup-files nil)


;; (global-set-key (kbd "C-o") 'open-next-line)
;; (global-set-key (kbd "M-o") 'open-previous-line)

;; (require 'dired+)
(setq dired-dwim-target t)
(setq dired-ls-F-marks-symlinks t)
(setq dired-listing-switches "-lh")

;; (use-package ibuffer
;;   :commands ibuffer
;;   :bind ("C-x C-b" . ibuffer)
;;   :config (progn
;;             (use-package ibuf-ext)
;;             (add-hook 'ibuffer-mode-hook
;;                       (lambda ()
;;                         (local-set-key (kbd "r" 'ibuffer-update))))))


(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode)
    (funcall mode -1)))

(setq ring-bell-function 'ignore)


;; (add-hook 'emacs-lisp-mode-hook #'xref-etags-mode)


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

(use-package hydra
  :ensure t)

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
  ;; (set-face-attribute 'aw-leading-char-face nil :foreground "#268bd2" :weight 'bold :height 3.0)
  ;; (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?k ?l))
  (add-to-list 'golden-ratio-extra-commands 'ace-window)
  (global-unset-key (kbd "C-x o")))

(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

(require 'rtj-clojure)
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
         :empty-lines 1)
        ("b" "Quick capture for brain dump" entry
         (file+headline "~/Desktop/brain-dump.org" "Brain Dump")
         "** %?")))
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

(use-package rust-mode
  :ensure t
  :defer t)

(use-package deft
  :ensure t
  :bind ("C-c d" . deft)
  :commands (deft)
  :config (setq deft-directory "~/Dropbox/org"
                deft-extensions '("org")))

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

(require 'rtj-binds)
(require 'rtj-hydras)

                                    
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



;; (put 'dired-find-alternate-file 'disabled nil)

