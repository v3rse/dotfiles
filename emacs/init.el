(setq inhibit-startup-message t) ; Don't show the damn splash screen
(setq visible-bell t)            ; Flash when the bell rings

;; Turn off some modes (UI elements mostly)
(menu-bar-mode -1)   ; file, edit, view etc
(tool-bar-mode -1)   ; the icons
(scroll-bar-mode -1) ; you know what this is

;; line numbers
(column-number-mode 1)
(global-display-line-numbers-mode 1)
;; dispable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; fonts
(set-face-attribute 'default nil :font "JetBrains Mono" :height 110)

;; a theme
;; (setq modus-themes-italic-constructs t                     ; italic fonts
;;       modus-themes-bold-constructs t                       ; bold fonts
;;       modus-themes-syntax '(green-strings yellow-comments) ; syntax colors
;;       modus-themes-headings                                 ; org headings
;;       '((1 . (rainbow background 1.4))
;; 	(2 . (rainbow background 1.3))
;; 	(3 . (rainbow bold 1.2))
;; 	(t . (semilight 1.1)))
;;       modus-themes-scale-headings t                        ; use the above org heading scalings
;;       modus-themes-org-blocks 'gray-background             ; use background for org blocks
;;       modus-themes-mode-line '(accented borderless padded) ; modeline customization
;;       modus-themes-region '(accented no-extend)            ; region selection
;;       modus-themes-completions                             ; minibuffer completions
;;       '((matches . (extrabold underline))
;; 	(selection . (semibold italic))))
;; (load-theme 'modus-vivendi t)

;; highlight the current line
(hl-line-mode 1)

;; stop the cursor from blinking
(blink-cursor-mode -1)

;; recent files (listed with M-x recentf-open-files)
(recentf-mode 1)

;; add some minibuffer prompt histroy
;; M-n/p or arrow keys to go up or down in the history
(setq history-length 25)
(savehist-mode 1)

;; minibuffer completion
(icomplete-mode 1)

;; remember and restore last cursor position of files
(save-place-mode 1)

;; avoid the wierd customization ui variable being inserted in this file
(setq custom-file (locate-user-emacs-file "custom-vars.el")) ; change the file location
(load custom-file 'noerror 'nomessage)                       ; load the file

;; don't pop up ui dialogs when prompting
(setq use-dialog-box nil)

;; automatically load file changes into open buffers
(global-auto-revert-mode 1)

;; same as above but for files listed in buffers like dired
(setq global-auto-revert-non-file-buffers t)


;; packages
;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; ivy + swiper + consel  (minibuffer suite)
;; ivy: completion in the minibuffer
;; swiper: uses ivy for search ui
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

; ivy-rich: more transformer for ivy and counsel
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

; counsel: ivy enhanced emacs functions
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

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

;; doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; doom themes
(use-package doom-themes
  :ensure t 
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; raibow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; evil: vim mode
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

;; org
(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; required for the next bit
  (require 'org-indent)
 
  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground 'unspecified :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-directory "~/notes/org/")
  (setq org-agenda-files '("tasks.org" "birthdays.org" "habits.org"))
  
  
  (efs/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

;; lsp
(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; the mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

;; ui elements
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; ivy additions
(use-package lsp-ivy)

;; deno
(setq lsp-clients-deno-config "./tsconfig.json")

;; typescript
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; completion with company mode (instead of C-M-i)
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


