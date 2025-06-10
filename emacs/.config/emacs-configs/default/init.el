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

    (set-face-attribute 'default nil :family mono-spaced-font :height 120 :weight 'medium)
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

  ;; tab for completion
  (setq tab-always-indent 'complete)

  
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

  :bind (("C-x C-r" . recentf-open-files)
	 ("C-x C-b" . ibuffer))
)

;; usability
(use-package eww)

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; lsp
(use-package eglot
  :ensure nil
  :custom
  (eglot-sync-connect 0) ; async do not block
  (eglot-autoshutdown t) ; shutdown after closing last managed buffer
  (eglot-report-progress nil) ; disable messages
  :hook ((prog-mode . eglot-ensure)
	 (eglot--managed-mode . eldoc-mode))
  :bind
  (:map eglot-mode-map
	("C-c e r" . eglot-rename)
	("C-c e a" . eglot-code-actions)
	("C-c e f" . eglot-format-buffer)
	("C-c e i" . eglot-find-implementation)))

(use-package eglot-booster
  :ensure t
  :after eglot
  :vc (eglot-booster :url "https://github.com/jdtsmith/eglot-booster.git"
		     :branch "main")
  :config (eglot-booster-mode))

;; debugger
(use-package dape
  :ensure t
  :hook ((kill-emacs . dape-breakpoint-save)
	 (after-init . dape-breakpoint-load))
  :config
  (dape-breakpoint-global-mode)
  (setq dape-buffer-window-arrangment 'right
	dape-inlay-hints t)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line))

(use-package repeat
  :config
  (repeat-mode))

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
                org-agenda-files '("inbox.org" "agenda.org" "projects.org" "notes.org")
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

(use-package ox-md
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

;; completions

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-resize t)
  (setq vertico-count 5)
  (setq vertico-scroll-margin 5))

(use-package marginalia
  :ensure t
  :hook (after-init . marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
        completion-category-overrides '((file (styles basic partial-completion))))

(use-package consult
  :ensure t
  :bind (
	 ;; general
	 ;; remaps
         ([remap recentf-open-files] . consult-recent-file)
         ([remap recentf] . consult-recent-file)
	 ([remap repeat-complex-command] . consult-complex-command) ; C-x M-:
         ([remap switch-to-buffer] . consult-buffer) ; C-x b
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window) ; C-x 4 b
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame) ; C-x 5 b
         ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab) ; C-x t b
         ([remap bookmark-jump] . consult-bookmark) ; C-x r b
         ([remap project-switch-to-buffer] . consult-project-buffer) ; C-x p b
	 ([remap yank-pop] . consult-yank-pop) ; M-y
	 ;; mode specific commands
	 ("C-c M-x" . consult-mode-command)
	 ;; search info manual
	 ("C-c i" . consult-info)
	 ;; themes
	 ("C-c T" . consult-theme)
	 
	 ;; search
	 ;; remaps
	 ([remap Info-search] . consult-info)
	 ;; recursive grep
	 ("M-s g" . consult-grep)
	 ;; search with git grep
	 ("M-s G" . consult-git-grep)
	 ;; search file name recursively
	 ("M-s f" . consult-find)
	 ;; search with fd
	 ("M-s F" . consult-fd)
	 ;; search current buffer
	 ("M-s l" . consult-line)
	 ;; search multiple buffers
	 ("M-s L" . consult-line-multi)
	 ;; search with ripgrep
	 ("M-s r" . consult-ripgrep)
	 ;; search keeping lines with matches
	 ("M-s k" . consult-keep-line)
	 ;; search and match line using overlays
	 ("M-s u" . consult-focus-lines)
	 ;; isearch integration
	 ("M-s e" . consult-isearch-history)

	 ;; goto
	 ;; remaps
	 ([remap imenu] . consult-imenu)
         ([remap goto-line] . consult-goto-line) ; M-g g or M-g M-g
	 ;; jump to compilation error
	 ("M-g e" . consult-compile-error)
	 ;; jump to flymake diagnostic
	 ("M-g f" . consult-flymake)
	 ;; jump to outline heading
	 ("M-g o" . consult-outline)
	 ;; jump to marker (buffer local)
	 ("M-g m" . consult-mark)
	 ;; jump to marker (global)
	 ("M-g k" . consult-global-mark)
	 ;; select from imenu project wide
	 ("M-g I" . consult-imenu-multi)
	 ;; jump to symbol
	 ("M-g s" . consult-eglot-symbols)))

(use-package consult-eglot
  :ensure t
  :config
  (consult-customize
   consult-eglot-symbols
   :initial (or (thing-at-point 'region t) (thing-at-point 'symbol t))))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
	 :map minibuffer-local-map
	 ("C-c C-c" . embark-collect)
	 ("C-c C-e" . embark-export)))

(use-package embark-consult
  :ensure t)

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map
	      ("<tab>" . corfu-complete))
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  :config
  (setq tab-always-indent 'complete)
  (setq corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package cape
  :ensure t
  :bind ("C-c p" . cape-prefix-map)
  :init
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent))

(use-package nerd-icons
  :ensure t
  :defer t) ;; only load when needed

(use-package nerd-icons-completion
  :ensure t
  :defer t
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
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
           :left-fringe-width 8
           :right-fringe-width 8))

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
  :hook ((dired-mode . diff-hl-dir-mode))
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

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?i)))

(use-package shackle
  :ensure t
  :custom
  (shackle-default-size .33)
  (shackle-select-reused-windows t)
  (shackle-inhibit-window-quit-on-same-windows t)
  (shackle-rules
   `((compilation-mode                :align below :popup t)
     (flymake-diagnostics-buffer-mode :align below :popup t)
     (magit-process-mode              :align below :popup t)
     ("*Async-native-compile-log*"    :align below :popup t)
     ("*Messages*"                    :align below :popup t)
     ("*eldoc*"                       :align below :popup t)
     ("*Process List*"                :align below :popup t :select t)
     ("*Warnings*"                    :align below :popup t :select t)
     ("*dired-check-process output*"  :align below :popup t :select t)
     ("*eshell*"                      :align below :popup t :select t)
     ("*vterm*"                       :align below :popup t :select t)
     (help-mode                       :align right  :popup t :select t :size 82)
     (helpful-mode                    :align right :popup t :select t :size 82)))
  :init
   (shackle-mode))

(use-package popper
  :ensure t
  :functions popper-group-by-project
  :bind (("C-`"   . popper-toggle)
         ("C-M-`"   . popper-cycle)
         ("C-x C-`" . popper-toggle-type))
  :custom
  (popper-display-control nil)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          ("Output\\*$" . hide)
          "\\*Async Shell Command\\*"
	  helpful-mode
          help-mode
          compilation-mode
	  flymake-diagnostics-buffer-mode
	  occur-mode
	  eldoc-mode
	  ;; terms and shells require both values
	  "^\\*eshell.*\\*$" eshell-mode
          "^\\*vterm.*\\*$" vterm-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package solaire-mode
  :ensure t
  :init
  (solaire-global-mode))

(use-package otpp
  :ensure t
  :after project
  :init
  (otpp-mode 1))

(use-package helpful
  :ensure t
  :bind (("C-h f" . #'helpful-callable)
	 ("C-h v" . #'helpful-variable)
	 ("C-h k" . #'helpful-key)
	 ("C-h x"  . #'helpful-command)
	 ("C-h F" . #'helpful-function)
         ("C-c C-d" . #'helpful-at-point)))

;; keybindings

(use-package which-key
  :ensure nil
  :init
  (which-key-mode 1))

(use-package god-mode
  :ensure t
  :init
  ;; enable god-mode support
  (which-key-enable-god-mode-support))

(use-package evil
  :ensure t
  :config
  (defun v3rse/vterm-copy-mode-evil ()
    (if (bound-and-true-p vterm-copy-mode)
	(evil-normal-state)
      ;; because evil-emacs-state doesn't work well with god-mode
      (evil-god-toggle-execute-in-god-off-state)))
  (add-hook 'vterm-copy-mode-hook #'my/vterm-copy-mode-evil)
  :init
  ;; required for evil-collection
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-god-toggle
  :ensure t
  :after (evil god-mode which-key)
  :vc (evil-god-toggle :url "https://github.com/jam1015/evil-god-toggle.git"
		       :branch "main")
  :init
  (setq evil-god-toggle-persist-visual 'always
        ;; Make god-mode global (applies to all buffers) instead of buffer-local:
        evil-god-toggle-global t)
  :config
  (evil-god-toggle-mode 1)

  ;; from evil normal to god mode
  (evil-define-key '(normal)
    evil-god-toggle-mode-map
    [escape] #'evil-god-toggle-execute-in-god-state)

  ;; from evil insert to god
  (evil-define-key '(insert)
    evil-god-toggle-mode-map
    (kbd "C-;") #'evil-god-toggle-execute-in-god-state)

  ;; from god back to previous evil state (just press ; to switch)
  (evil-define-key 'god
    evil-god-toggle-mode-map
    (kbd "C-;") #'evil-change-to-previous-state)

  ;; use escape in both god modes to normal
  (evil-define-key '(god god-off)
    evil-god-toggle-mode-map
    [escape] (lambda ()
               (interactive)
               (evil-god-toggle-stop-choose-state 'normal)))

    ;; flip-flop between on/of god mode in any state
    (evil-define-key '(god god-off)
      evil-god-toggle-mode-map
      (kbd "C-M-;") #'evil-god-toggle-god-toggle)

    ;; enter god mode for one command
    (evil-define-key '(normal insert)
      evil-god-toggle-mode-map
      (kbd "C-,") #'evil-god-toggle-once))

;; (use-package meow
;;   :ensure t
;;   :demand t
;;   :config
;;   (defun meow-setup ()
;;     (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;     (meow-motion-define-key
;;      '("j" . meow-next)
;;      '("k" . meow-prev)
;;      '("<escape>" . ignore))
;;     (meow-leader-define-key
;;      ;; Use SPC (0-9) for digit arguments.
;;      '("1" . meow-digit-argument)
;;      '("2" . meow-digit-argument)
;;      '("3" . meow-digit-argument)
;;      '("4" . meow-digit-argument)
;;      '("5" . meow-digit-argument)
;;      '("6" . meow-digit-argument)
;;      '("7" . meow-digit-argument)
;;      '("8" . meow-digit-argument)
;;      '("9" . meow-digit-argument)
;;      '("0" . meow-digit-argument)
;;      '("/" . meow-keypad-describe-key)
;;      '("?" . meow-cheatsheet))
;;     (meow-normal-define-key
;;      '("0" . meow-expand-0)
;;      '("9" . meow-expand-9)
;;      '("8" . meow-expand-8)
;;      '("7" . meow-expand-7)
;;      '("6" . meow-expand-6)
;;      '("5" . meow-expand-5)
;;      '("4" . meow-expand-4)
;;      '("3" . meow-expand-3)
;;      '("2" . meow-expand-2)
;;      '("1" . meow-expand-1)
;;      '("-" . negative-argument)
;;      '(";" . meow-reverse)
;;      '("," . meow-inner-of-thing)
;;      '("." . meow-bounds-of-thing)
;;      '("[" . meow-beginning-of-thing)
;;      '("]" . meow-end-of-thing)
;;      '("a" . meow-append)
;;      '("A" . meow-open-below)
;;      '("b" . meow-back-word)
;;      '("B" . meow-back-symbol)
;;      '("c" . meow-change)
;;      '("d" . meow-delete)
;;      '("D" . meow-backward-delete)
;;      '("e" . meow-next-word)
;;      '("E" . meow-next-symbol)
;;      '("f" . meow-find)
;;      '("g" . meow-cancel-selection)
;;      '("G" . meow-grab)
;;      '("h" . meow-left)
;;      '("H" . meow-left-expand)
;;      '("i" . meow-insert)
;;      '("I" . meow-open-above)
;;      '("j" . meow-next)
;;      '("J" . meow-next-expand)
;;      '("k" . meow-prev)
;;      '("K" . meow-prev-expand)
;;      '("l" . meow-right)
;;      '("L" . meow-right-expand)
;;      '("m" . meow-join)
;;      '("n" . meow-search)
;;      '("o" . meow-block)
;;      '("O" . meow-to-block)
;;      '("p" . meow-yank)
;;      '("q" . meow-quit)
;;      '("Q" . meow-goto-line)
;;      '("r" . meow-replace)
;;      '("R" . meow-swap-grab)
;;      '("s" . meow-kill)
;;      '("t" . meow-till)
;;      '("u" . meow-undo)
;;      '("U" . meow-undo-in-selection)
;;      '("v" . meow-visit)
;;      '("w" . meow-mark-word)
;;      '("W" . meow-mark-symbol)
;;      '("x" . meow-line)
;;      '("X" . meow-goto-line)
;;      '("y" . meow-save)
;;      '("Y" . meow-sync-grab)
;;      '("z" . meow-pop-selection)
;;      '("'" . repeat)
;;      '("<escape>" . ignore)))
;;   ;; HACK: allows 'a' to work as expected
;;   ;; https://github.com/meow-edit/meow/discussions/87
;;   (setq meow-use-cursor-position-hack t
;;       meow-use-enhanced-selection-effect t)
;;   (meow-setup)
;;   (meow-global-mode 1))


;; -- Server
(server-start)
