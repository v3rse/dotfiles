;; -*- lexical-binding: t; -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; general
(use-package emacs
  :ensure nil

  :config
  ;; font selection
  (let ((mono-spaced-font "Iosevka")
        (proportionately-spaced-font "Iosevka Etoile"))

    (set-face-attribute 'default nil :family mono-spaced-font :height 130 :weight 'medium)
    (when (eq system-type 'darwin)
      (set-face-attribute 'default nil :family mono-spaced-font :height 140))
  
    (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
    (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

  ;; recent files list size
  (setq recentf-max-menu-items 25)
  
  ;;diary
  (setq diary-file "~/org/emacs-diary")
  
  ;; locks, autosave and backup
  (setq make-backup-files nil)
  (setq create-lockfiles nil)
  (make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
  (setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

  ;; custom files location
  (setq custom-file (locate-user-emacs-file "custom.el"))
  (load custom-file 'noerror 'nomessage)

  (setq-default fill-column 90)

  
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
  
  ;; match bracket pairs
  (electric-pair-mode 1)
  ;; auto indent
  (electric-indent-mode 1)
  
  ;; history and completions
  (savehist-mode 1)
  (recentf-mode 1)

  ;; help
  (which-key-mode 1)

  :bind (("C-x C-r" . recentf-open-files)
	 ("C-x C-b" . ibuffer))
)

;; usability
(use-package eww)

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package eglot
  :ensure nil
  :config
  (setq eglot-autoshutdown t)
  :hook ((python-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (web-mode . eglot-ensure)
         (html-mode . eglot-ensure)
         (css-mode . eglot-ensure)))

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


(use-package org
  :config (setq org-directory "~/org/"
                org-default-notes-file "~/org/inbox.org"
                org-agenda-files '("inbox.org" "agenda.org" "projects.org")
		org-archive-location "~/org/archive/%s_archive::datetree/"
                org-ellipsis " ... "
                org-tags-column -80
                org-log-into-drawer t
                org-hide-emphasis-markers t
                org-agenda-start-day nil
                org-log-done 'time
		org-agenda-include-diary t
		org-refile-use-outline-path t
		org-outline-path-complete-in-steps nil
		org-M-RET-may-split-line '((default . nil))
		org-insert-heading-respect-content t)
  ;; keywords
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "PROJ(p)" "|" "DONE(d)" "CNCL(c)")))
  (setq org-todo-keyword-faces
	'(("TODO" . (:inherit (bold font-lock-builtin-face org-todo)))
	  ("HOLD" . (:inherit (bold warning org-todo)))
	  ("DONE" . (:inherit (bold org-todo)))
	  ("PROJ" . (:inherit (bold font-lock-keyword-face org-todo)))
	  ("NEXT" . (:inherit (bold font-lock-constant-face org-todo)))
	  ("CNCL" . (:inherit (bold warning org-todo)))))
  (setq org-modern-todo-faces
	'(("TODO" . (:inherit (bold font-lock-builtin-face org-modern-todo)))
	  ("HOLD" . (:inherit (bold warning org-modern-todo)))
	  ("DONE" . (:inherit (bold org-modern-todo)))
	  ("PROJ" . (:inherit (bold font-lock-keyword-face org-modern-todo)))
	  ("NEXT" . (:inherit (bold font-lock-constant-face org-modern-todo)))
	  ("CNCL" . (:inherit (bold warning org-modern-todo)))))
  ;; capture templates
  (setq org-capture-templates
        `(("t" "Task" entry (file+headline "inbox.org" "Tasks")
           ,(string-join '("* TODO %?"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":CATEGORY: Task"
                           ":END:")
                         "\n"))
          ("n" "Note" entry (file+headline "inbox.org" "Notes")
           ,(string-join '("* %?"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":CATEGORY: Note"
                           ":END:")
                         "\n"))
          ("m" "Meeting" entry (file+headline "inbox.org" "Meetings")
           ,(string-join '("* %? :MEETING"
                           "<%<%Y-%m-%d %a %H:00>>"
                           ""
                           "/Met with: /")
                         "\n"))
	  ("b" "Bookmark" entry (file+headline "inbox.org" "Bookmarks")
	   ,(string-join '("* %:description"
			 ":PROPERTIES:"
			 ":CREATED: %U"
			 ":CATEGORY: Bookmark"
			 ":END:"
			 "%:link"
			 ""
			 "%i"
			 ""
			 "%?")
			 "\n")
	   :empty-lines 1)
	  ("j" "Journal" entry (file+datetree "journal.org")
	   "* %?\nEntered on %U\n %i\n %a")
	  ))

  ;; agenda
  (setq org-agenda-prefix-format '((agenda . "  %i %?-12t")
                                   (todo . "  %i")
                                   (tags . "  %i %-12:c")
                                   (search . "  %i %-12:c")))
	
  (setq org-agenda-custom-commands
        '(("g" "Get Things Done (GTD)"
                ((agenda ""
                         ((org-agenda-span 'day)
                          (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'deadline))
                          (org-deadline-warning-days 0)))
                 (todo "TODO"
                        ((org-agenda-overriding-header "Refile")
                        (org-agenda-files '("inbox.org"))))
                (todo "NEXT"
                        ((org-agenda-overriding-header "In Progress")
                                (org-agenda-files '("someday-maybe.org"
                                                "projects.org"
                                                "agenda.org"))))
                (todo "PROJ"
                        ((org-agenda-overriding-header "Projects")
                                (org-agenda-files '("projects.org"))))
                (todo "TODO"
                      ((org-agenda-overriding-header "One-off Tasks")
                       (org-agenda-files '("agenda.org"))
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                 (agenda nil
                         ((org-agenda-entry-types '(:deadline))
                          (org-deadline-warning-days 7)
                          (org-agenda-overriding-header "\nDeadlines\n")))
                 (tags "CLOSED>=\"<today>\""
                       ((org-agenda-overriding-header "\nCompleted today\n")))
                 ))))
 ;; module
 (add-to-list 'org-modules 'org-habit)

 ;; refile
 (setq v3rse/org-refile-target-files '("agenda.org"
                                      "projects.org"
                                      "someday-maybe.org"
                                      "notes.org"))

 (setq v3rse/org-refile-file-paths
      (let (result)
        (dolist (file v3rse/org-refile-target-files result)
          (push (expand-file-name file org-directory) result))))

 (setq org-refile-targets
      '((nil :maxlevel . 9)
        (v3rse/org-refile-file-paths :maxlevel . 9)))

 :hook ((org-mode-hook . visual-line-mode))
 :bind (("C-c a" . org-agenda)
	("C-c c" . org-capture)
	("C-c l" . org-store-link)))

(use-package org-attach
  :after org
  :ensure nil)

(use-package org-protocol
  :after org
  :ensure nil)

(use-package org-tempo
  :after org
  :ensure nil
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("q" . "quote")))

(use-package newst-reader
  :ensure nil
  :custom
  (custom-set-faces
   '(newsticker-feed-face ((t (:inherit ef-themes-heading-1 :height 1.4))))
   '(newsticker-new-item-face ((t (:inherit ef-themes-heading-4 :height 1.2)))))
  :hook ((newsticker-mode-hook . variable-pitch-mode))
  :config
  (setq newsticker-frontend 'newsticket-plainview)
  (setq newsticker-use-full-width nil)
  (setq newsticker-url-list
      '(
          ("Euronews" "https://www.euronews.com/rss")
          ("Allsides News" "https://www.allsides.com/rss/news")
	  ("arstechnica" "https://feeds.arstechnica.com/arstechnica/index")
          ("Polygon" "https://www.polygon.com/rss/index.xml")
          ("Recurse" "https://blaggregator.recurse.com/atom.xml?token=561d4f124fc342d78c6e25da65dfd69a")
          ("Hacker News" "https://news.ycombinator.com/rss")
          ("Planet Emacs" "https://planet.emacslife.com/atom.xml")
          ("Lobsters" "https://lobste.rs/rss")
	)
     ) 
  )

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

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-tomorrow-night t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

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
  :config
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
           :mode-line-inactive spacious-padding-subtle-mode-line-inactive))
  :init
  (spacious-padding-mode 1))

(use-package keycast
  :ensure t
  :init
  (keycast-mode-line-mode 1))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit)))

(use-package aidermacs
  :bind (("C-c x" . aidermacs-transient-menu))
  :config
  ; Set Ollama API endpoint (no API key needed)
  (setenv "OLLAMA_API_BASE" "http://localhost:11434")
  (setq aidermacs-aider-command "~/.local/bin")
  :custom
  ; See the Configuration section below
  (aidermacs-default-model "ollama_chat/deepseek-r1:14b"))

(use-package org-modern
  :ensure t
  :defer t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode 1))

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
  (setq org-pomodoro-length 45
	org-pomodoro-short-break-length 15))

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package vterm
  :ensure t
  :bind (("C-c t" . vterm)))

(use-package popper
  :ensure t ;
  :bind (("C-`"   . popper-toggle)
         ("C-M-`"   . popper-cycle)
         ("C-x C-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          ("Output\\*$" . hide)
          "\\*Async Shell Command\\*"
	  helpful-mode
          help-mode
          compilation-mode
	  occur-mode
	  eldoc-mode
	  ;; terms
	  "^\\*eshell.*\\*$" eshell-mode
          "^\\*shell.*\\*$"  shell-mode
          "^\\*term.*\\*$"   term-mode
          "^\\*vterm.*\\*$"  vterm-mode)
	 popper-window-height 0.33) ;; 33% of display
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package helpful
  :ensure t
  :bind (("C-h f" . #'helpful-callable)
	 ("C-h v" . #'helpful-variable)
	 ("C-h k" . #'helpful-key)
	 ("C-h x"  . #'helpful-command)
	 ("C-h F" . #'helpful-function)
         ("C-c C-d" . #'helpful-at-point)))

(use-package god-mode
  :ensure t
  :bind (("<escape>" . god-local-mode)))

;; -- Server
(server-start)
