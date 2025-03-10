(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; general
(use-package emacs
  :ensure nil

  :config
  ;; font selection
  (let ((mono-spaced-font "Iosevka")
        (proportionately-spaced-font "Iosevka Etoile"))

    (set-face-attribute 'default nil :family mono-spaced-font :height 100)
    (when (eq system-type 'darwin)
      (set-face-attribute 'default nil :family mono-spaced-font :height 140))
  
    (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
    (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

  ;; custom files location
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file 'noerror 'nomessage)
  
  :init
  ;; gui bars
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (when scroll-bar-mode
    (scroll-bar-mode -1))
  (indent-tabs-mode -1)

  ;; files and buffers
  (global-hl-line-mode 1)
  (global-auto-revert-mode 1)
  (global-completion-preview-mode 1)

  ;; history and completions
  (savehist-mode 1)
  (recentf-mode 1)
  (fido-vertical-mode 1) ;; replaces vertico
)

;; usability
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))


(use-package window
  :ensure nil
  :custom
  (display-buffer-alist
   '(
      ;; auxiliary buffers: bottom 25%
      ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

      ;; flymake and completions: bottom 25%
      ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
      )))

(use-package dired
  :ensure nil
  :commands (dired)
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . hl-line-mode))
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t))

;; -- external --
(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package treesit-auto
  :ensure t
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)

  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package ef-themes
  :ensure t
  :config
  (load-theme 'ef-dream))

(use-package nerd-icons
  :ensure t
  :defer t) ;; only load when needed

(use-package nerd-icons-completion
  :ensure t
  :defer t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :defer t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :defer t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  :defer t
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)  
  (doom-modeline-project-detection 'project)           
  (doom-modeline-buffer-name t)                        
  (doom-modeline-vcs-max-length 25)                    
  :config
  (setq doom-modeline-icon t)                        
  :hook
  (after-init . doom-modeline-mode))

(use-package spacious-padding
  :ensure t
  :if (display-graphic-p)
  :hook (after-init . spacious-padding-mode)
  :bind ("<f8>" . spacious-padding-mode)
  :init
  ;; These are the defaults, but I keep it here for visiibility.
  (setq spacious-padding-widths
        '( :internal-border-width 30
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :left-fringe-width 20
           :right-fringe-width 20))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as
  ;; it is very flexible.
  (setq spacious-padding-subtle-mode-line
        '( :mode-line-active spacious-padding-subtle-mode-line
           :mode-line-inactive spacious-padding-subtle-mode-line-inactive)))


