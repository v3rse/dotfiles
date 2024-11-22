(setq gc-cons-threshold (* 50 1000 1000))
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(setq large-file-warning-threshold (* 100 1024 1024)) ;; 100MB

(require 'package)
(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(require 'use-package)
(setq use-package-always-ensure t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Welcome to v3rsemacs: An idea machine")
(setq visible-bell t)
(setq use-dialog-box nil)

;; highlight the current line
(hl-line-mode 1)

;; line and column numbers:
(global-display-line-numbers-mode 1)
;; don't show line numbers for the following modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; show column number
(column-number-mode 1)

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

;; match bracket pairs only in programming modes
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; auto indent
(electric-indent-mode 1)

;; stop the cursor from blinking
(blink-cursor-mode -1)

;; minibuffer prompt history
(setq history-length 25)
(savehist-mode 1)

;; reload buffer when file changes on system
(global-auto-revert-mode 1)
;; reloads buffer when content changes on systeme
(setq global-auto-revert-non-file-buffers t)

(set-face-attribute 'default nil :font "JetBrains Mono" :height 110)
(set-face-attribute 'variable-pitch nil :family "ETBembo" :height 200)

;; doom emacs themes
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(load-theme 'doom-tomorrow-night t)

(use-package counsel
	:ensure t
	:bind (("M-x" . counsel-M-x)
	       ("C-x b" . counsel-ibuffer)
	       ("C-x C-f" . counsel-find-file)
	       :map minibuffer-local-map
	       ("C-r" . 'counsel-minibuffer-history)))

  (use-package ivy
	:diminish
	:bind (("C-s" . swiper)
		:map ivy-minibuffer-map
		("TAB" . ivy-alt-done)
		("C-l" . ivy-alt-done)
		("C-j" . ivy-next-line)
		("C-k" . ivy-previous-line)
		:map ivy-switch-buffer-map
		("C-k" . ivy-previous-line)
		("C-l" . ivy-done)
		("C-d" . ivy-switch-buffer-kill)
		:map ivy-reverse-i-search-map
		("C-k" . ivy-previous-line)
		("C-d" . ivy-reverse-i-search-kill))
	:config
	(ivy-mode 1))

(use-package ivy-rich
    :init
    (ivy-rich-mode 1))

  (use-package swiper
	:ensure t)

  (use-package lsp-ivy
    :ensure t
    :commands lsp-ivy-workspace-symbol
    :bind ("C-c s" . lsp-ivy-workspace-symbol))

  ; helpful: more contextual docs with counsel
  (use-package helpful
    :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
    :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-key] . helpful-key))

;; Customize the modeline
(use-package minions
  :ensure t
  :config
  (minions-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map)))

(use-package js2-mode
  :mode "\\.js\\'"
  :hook (js2-mode . lsp))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . lsp))

(use-package lsp-mode
  :hook ((js-mode . lsp)
	 (typescript-mode . lsp))
  :config (setq lsp-clients-deno-config "./deno.json")
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(lsp-headerline-breadcrumb-mode -1)

(use-package company
  :hook (after-init . global-company-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
	 (typescript-mode . prettier-js-mode)))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; addition evil stuff
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package org
  :config
  (setq org-directory "~/notes/org/")
  (setq org-agenda-files
	'("tasks.org" "habits.org" "birthdays.org"))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-archive-location "~/notes/org/archive.org::datetree/*")
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))


;; (use-package org-modern
;;   :init
;;   (global-org-modern-mode)
;;   :config
;;   (set-face-attribute 'org-modern-symbol nil :family "Iosevka"))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(use-package vterm
  :ensure t)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-custom-commands
   '(("A" "Agenda and all NEXT maintainance"
      ((agenda ""
	       ((org-agenda-span 'day)))
       (tags-todo "MAINTAINANCE+TODO=\"NEXT\"+TODO=\"TODO\"" nil))
      nil)))
 '(org-agenda-files
   '("~/src/personal/codex-org-mode/rainer-course/tutorial.org" "/home/v3rse/notes/org/archive/notes/example.org"))
 '(org-babel-load-languages '((shell . t) (dot . t) (awk . t) (emacs-lisp . t)))
 '(org-capture-templates
   '(("w" "Private templates")
     ("wt" "TODO entry" entry
      (file+headline "~/src/personal/codex-org-mode/rainer-course/tutorial.org" "Work Capture")
      (file "~/src/personal/codex-org-mode/rainer-course/tpl-work-todo.txt"))))
 '(org-clock-into-drawer "CLOCKING")
 '(org-export-backends '(ascii beamer html icalendar latex odt))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(org-publish-project-alist
   '(("joe" :base-directory "~/src/personal/codex-org-mode/rainer-course/joe/" :publishing-directory "~/public_html" :publishing-function org-html-publish-to-html :section-numbers nul :with-toc nil)))
 '(org-refile-targets nil)
 '(package-selected-packages
   '(vterm go-mode which-key visual-fill-column typescript-mode rainbow-delimiters projectile prettier-js org-super-agenda org-modern org-bullets minions magit lsp-ui lsp-ivy js2-mode ivy-rich helpful flycheck exec-path-from-shell evil-collection doom-themes doom-modeline counsel company-box)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
