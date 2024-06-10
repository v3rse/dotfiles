(setq gc-cons-threshold (* 50 1000 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(setq large-file-warning-threshold (* 100 1024 1024)) ;; 100MB

(setq viper-mode t)
(require 'viper)
(setq viper-expert-mode 3)

;; Basic UI configuration
 (menu-bar-mode -1)
 (tool-bar-mode -1)
 (scroll-bar-mode -1)
 (setq inhibit-startup-screen t)
 (setq initial-scratch-message "")
 (global-display-line-numbers-mode 1)
 (setq display-line-numbers 'relative)
 (set-frame-font "JetBrains Mono-12" nil t)

 ;; Load a simple theme
 (load-theme 'modus-operandi t)

 ;; Simple modeline
 (setq-default mode-line-format
   '("%e"
     mode-line-front-space
     mode-line-buffer-identification
     " "
     mode-line-position
     (vc-mode vc-mode)
     " "
     mode-line-modes
     mode-line-misc-info))


;; Simple fringe configuration
(set-fringe-mode '(8 . 0))

;; recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; save place
(save-place-mode 1)

;; follow symlinks under version control(the linked file is not tracked)
(setq vc-follow-symlinks t)

;; enable clipbard
(setq select-enable-clipboard t)

;; match bracket pairs
(electric-pair-mode 1)

;; auto indent
(electric-indent-mode 1)

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (web-mode . eglot-ensure)
         (html-mode . eglot-ensure)
         (css-mode . eglot-ensure)))

(use-package treesit
  :config
  (setq major-mode-remap-alist
        '((c-mode          . c-ts-mode)
          (c++-mode        . c++-ts-mode)
          (css-mode        . css-ts-mode)
          (java-mode       . java-ts-mode)
          (javascript-mode . js-ts-mode)
          (js-json-mode    . json-ts-mode)
          (python-mode     . python-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (ruby-mode       . ruby-ts-mode)
          (go-mode         . go-ts-mode)
          (rust-mode       . rust-ts-mode))))

;; Basic Eglot settings
(setq eglot-autoshutdown t)

(use-package project
  :bind (("C-x p f" . project-find-file)
	 ("C-x p p" . project-switch-project)
	 ("C-x p s" . project-search)
	 ("C-x p k" . project-kill-buffers)
	 ("C-x p t" . project-shell)
	 ("C-x p d" . project-dired)))

(use-package xref
  :bind (("M-?" . xref-find-references)
         ("M-." . xref-find-definitions)
         ("M-," . xref-go-back)))

(global-set-key (kbd "C-x t") 'term)

(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c g") 'gdb)
