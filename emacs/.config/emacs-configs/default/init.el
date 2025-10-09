;; -*- lexical-binding: t; -*-

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq use-package-always-ensure t)

;; general
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package emacs
  :ensure nil

  :config
  ;; font selection
  (let ((mono-spaced-font "Aporetic Serif Mono")
        (proportionately-spaced-font "Aporetic Sans"))

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

  ;; short answers for prompts
  (setq use-short-answers t)

  ;; use emacs (loopback) for epg/epa (gpg) pinentry
  (setq epg-pinentry-mode 'loopback)

  ;; browser
  (when (eq system-type 'darwin)
    (setq browse-url-browser-function 'browse-url-generic
	    browse-url-generic-program "/Applications/Firefox.app/Contents/MacOS/firefox"))
  
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

  :hook
  ((prog-mode . display-line-numbers-mode)
   (text-mode . visual-line-mode))

  :bind (("C-x C-r" . recentf-open-files)
	 ("C-x C-b" . ibuffer)))

;; usability
(use-package eww
  :ensure nil)

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

;; lsp
(defun v3rse/set-default-margins ()
  (setq left-margin-width 3)
  (set-window-buffer (selected-window) (current-buffer)))

(use-package eglot
  :ensure nil
  :custom
  (eglot-sync-connect 0) ; async do not block
  (eglot-autoshutdown t) ; shutdown after closing last managed buffer
  (eglot-report-progress nil) ; disable messages
  :hook
  ((prog-mode . eglot-ensure)
   (eglot-managed-mode . eldoc-mode)
   (eglot-managed-mode . v3rse/set-default-margins))
  :bind
  (:map eglot-mode-map
	("C-c e r" . eglot-rename)
	("C-c e a" . eglot-code-actions)
	("C-c e f" . eglot-format-buffer)
	("C-c e i" . eglot-find-implementation)))

(use-package eglot-booster
  :after eglot
  :vc (eglot-booster :url "https://github.com/jdtsmith/eglot-booster.git"
		     :branch "main")
  :config (eglot-booster-mode))

(use-package flyover
  :vc (flyover :url "https://github.com/v3rse/flyover.git"
	       :branch "main"
	       :rev "77f5c7b3f758a671eeb887a20c7eeb4ad717529e")
  :hook
  (flymake-mode . flyover-mode)
  :config
  (setq flyover-use-theme-colors t
	flyover-checkers '(flymake)
	flyover-text-tint 'darker
	flyover-text-tint-percent 100))

;; debugger
(use-package dape
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
  :ensure nil
  :demand t
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
		org-agenda-include-diary nil
		org-refile-use-outline-path t
		org-outline-path-complete-in-steps nil
		org-M-RET-may-split-line '((default . nil))
		org-insert-heading-respect-content t
		org-indent-mode t
		org-agenda-sticky t)
  ;; keywords
  (setq org-todo-keywords '((sequence "TODO(t)" "STRT(s!)" "NEXT(n!)" "HOLD(h@)" "LOOP(l)" "PROJ(p)" "|" "DONE(d!)" "CNCL(c@)")))
  (setq org-todo-keyword-faces
	'(("TODO" . (:inherit (bold font-lock-builtin-face org-todo)))
	  ("HOLD" . (:inherit (bold warning org-todo)))
	  ("DONE" . (:inherit (bold org-todo)))
	  ("PROJ" . (:inherit (bold font-lock-keyword-face org-todo)))
	  ("LOOP" . (:inherit (bold font-lock-keyword-face org-todo)))
	  ("NEXT" . (:inherit (bold font-lock-constant-face org-todo)))
	  ("STRT" . (:inherit (bold font-lock-property-name-face org-todo)))
	  ("CNCL" . (:inherit (bold font-lock-doc-face org-todo)))))
  (setq org-modern-todo-faces
	'(("TODO" . (:inherit (bold font-lock-builtin-face org-modern-todo)))
	  ("HOLD" . (:inherit (bold warning org-modern-todo)))
	  ("DONE" . (:inherit (bold org-modern-todo)))
	  ("PROJ" . (:inherit (bold font-lock-keyword-face org-modern-todo)))
	  ("LOOP" . (:inherit (bold font-lock-keyword-face org-modern-todo)))
	  ("NEXT" . (:inherit (bold font-lock-constant-face org-modern-todo)))
	  ("STRT" . (:inherit (bold font-lock-property-name-face org-modern-todo)))
	  ("CNCL" . (:inherit (bold font-lock-doc-face org-modern-todo)))))
  ; priority A is already red
  (setq org-modern-priority-faces
	(quote ((?B (:inherit warning :inverse-video t))
		(?C (:inherit org-todo :inverse-video t)))))
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
                        ((org-agenda-overriding-header "📥 Refile")
                         (org-agenda-files '("inbox.org"))))
		 (tags "EFFORT>\"0:00\"+EFFORT<\"0:20\""
			((org-agenda-overriding-header "⚡ Quick Hits")
			 (org-agenda-files '("agenda.org"))
			 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE")))))
                (todo "NEXT|STRT"
                        ((org-agenda-overriding-header "🚧 In Progress")
                                (org-agenda-files '("projects.org" "agenda.org"))))
                (todo "TODO"
                      ((org-agenda-overriding-header "✅ One-offs")
                       (org-agenda-files '("agenda.org"))
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                 (agenda nil
                         ((org-agenda-entry-types '(:deadline))
                          (org-deadline-warning-days 7)
                          (org-agenda-overriding-header "\n❗Deadlines\n")))
                 (tags "CLOSED>=\"<today>\""
                       ((org-agenda-overriding-header "\n🎉 Completed today\n")))
                 ))

	  ("p" "Projects"
	   ((todo "PROJ"
                        ((org-agenda-overriding-header "🚀 Projects")
                                (org-agenda-files '("projects.org"))))

	    ))
	  ))
  
 ;; habits
 (add-to-list 'org-modules 'org-habit)
 
 (setq org-habit-completed-glyph ?✅)
 (setq org-habit-incompleted-glyph ?❌)
 (setq org-habit-skipped-glyph ?➖)

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

;; news
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
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))

(use-package treesit-auto
  :after emacs
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

(use-package nix-ts-mode
  :ensure t
  :mode "\\.nix\\'")

;; completions

(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t)
  (setq vertico-resize t)
  (setq vertico-count 15)
  (setq vertico-scroll-margin 5))

(use-package marginalia
  :hook (after-init . marginalia-mode)
  :config
  (setq marginalia-max-relative-age 0))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic))
        completion-category-overrides '((file (styles basic partial-completion))))

(use-package consult
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
	 ;; registers
	 ("M-'" . consult-register-store)
	 ("M-#" . consult-register-load)

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
	 ("M-g s" . consult-eglot-symbols))

  :init

  ;; tweak register preview
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)

  ;; use consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

(use-package consult-eglot
  :config
  (consult-customize
   consult-eglot-symbols
   :initial (or (thing-at-point 'region t) (thing-at-point 'symbol t))))

(use-package embark
  :bind (("C-." . embark-act)
	 ("M-." . embark-dwim)
	 ("C-h B" . embark-bindings)
	 :map minibuffer-local-map
	 ("C-c C-c" . embark-collect)
	 ("C-c C-e" . embark-export)))

(use-package embark-consult)

(use-package corfu
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
  :bind ("C-c p" . cape-prefix-map)
  :init
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent))

(use-package nerd-icons
  :defer t) ;; only load when needed

(use-package nerd-icons-completion
  :defer t
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :defer t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(defun clean-load-theme (nt)
  (unless (seq-empty-p custom-enabled-themes) 
    (dolist (ot custom-enabled-themes)
	(disable-theme ot)))
  (load-theme nt t))

(defun set-light-theme ()
  "set the light theme"
  (interactive)
  ;; (clean-load-theme 'doom-tomorrow-day)
  (setq catppuccin-flavor 'frappe)
  (catppuccin-reload))

(defun set-dark-theme ()
  "set the dark theme"
  (interactive)
   ;; (if (eq system-type 'darwin)
   ;; 	(clean-load-theme 'doom-opera)
   ;;   (clean-load-theme 'doom-tomorrow-night))
  (setq catppuccin-flavor 'mocha)
   (catppuccin-reload))

(defun set-theme-by-time ()
  "Set a light theme for day and a dark theme for night."
  (interactive)
  (let ((hour (string-to-number (format-time-string "%H"))))
    ;; Use light theme between 7 AM (7) and 7 PM (19)
    (if (and (>= hour 7) (< hour 19))
	(set-light-theme)
	(set-dark-theme))))

(use-package catppuccin-theme)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; changes theme base on time
  (set-theme-by-time)
  ;; update every hour
  (run-with-timer 0 3600 'set-theme-by-time))

(use-package doom-modeline
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
  :if (display-graphic-p)
  :hook (after-init . spacious-padding-mode)
  :bind ("<f8>" . spacious-padding-mode)
  :config
  ;; These are the defaults, but I keep it here for visiibility.
  (setq spacious-padding-widths
        '( :internal-border-width 1
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 10
           :left-fringe-width 16
           :right-fringe-width 16))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as
  ;; it is very flexible.
  (setq spacious-padding-subtle-mode-line
        '( :mode-line-active spacious-padding-subtle-mode-line
           :mode-line-inactive spacious-padding-subtle-mode-line-inactive))
  :init
  (spacious-padding-mode 1))

(use-package keycast
  :init
  (keycast-mode-line-mode 1))

;; version control

(use-package magit
  :bind (("C-c g" . magit)))

(use-package diff-hl
  :hook
  ((dired-mode . diff-hl-dir-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode 1)
  :config
  (diff-hl-margin-mode 1))

;; ai

(use-package gptel
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-default-input-format 'org)
  (defvar v3rse/gptel-ollama-host "localhost"
    "The ollama host server address for gptel")

  (defun v3rse/gptel-use-claude ()
    "Switch to a claude backend for gptel"
    (interactive)
    (setq gptel-model 'claude-3-sonnet-20240229
	  gptel-backend (gptel-make-anthropic "Claude"
			  :stream t
			  :key (cadr (auth-source-user-and-password "api.anthropic.com" "apikey")))))

  (defun v3rse/gptel-use-ollama ()
    "Switch to a ollama backend for gptel"
    (interactive)
    (setq gptel-model 'gemma3:12b
	  gptel-backend (gptel-make-ollama "Ollama"
			  :host (format "%s:11434" v3rse/gptel-ollama-host)
			  :stream t
			  :models '(deepseek-r1:14b gemma3:12b))))

  (defun v3rse/gptel-use-chatgpt ()
    "Switch to a chatgpt backend for gptel"
    (interactive)
    (setq gptel-model (default-value 'gptel-model)
	  gptel-backend (default-value 'gptel-backend)))

  :init
  (require 'gptel-integrations))

;; required by acp
(use-package shell-maker
  :ensure t)

(use-package acp
  :vc (:url "https://github.com/xenodium/acp.el"))

(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell")
  (setq agent-shell-google-authentication
      (agent-shell-google-make-authentication :login t)))

(use-package org-modern
  :defer t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(use-package org-pomodoro
  :commands (org-pomodoro)
  :config
  (setq org-pomodoro-length 45
	org-pomodoro-short-break-length 15))

(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600))

(use-package ansi-color
  :demand t
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(use-package vterm
  :hook
  (vterm-mode . compilation-shell-minor-mode)
  ;; full screen terminal instead of the 1/3 screen one
  ;; shackle rule applies to multi-vterm(vterminal) window
  :bind (("C-c t" . vterm)))

(defun v3rse/multi-vterm-toggle-dwim ()
    "Toggle the vterm window.
When in a project, toggle a `multi-vterm-project' terminal. When outside
a project, call `multi-vterm-dedicated-toggle'."
    (interactive)
    (if-let* ((buf-name (and (multi-vterm-project-root) (multi-vterm-project-get-buffer-name))))
        (if-let* ((buf (get-buffer buf-name))
                  ((buffer-live-p buf)))
            (if-let* ((win (get-buffer-window buf))) ; The project's vterm already exists, toggle it's window
                (delete-window win)
              (pop-to-buffer buf))
          (multi-vterm-project))
      (multi-vterm-dedicated-toggle)))

(use-package multi-vterm
  :bind (([remap project-shell] . multi-vterm-project)
         ([f1] . v3rse/multi-vterm-toggle-dwim)
         :map vterm-mode-map ([f1] . v3rse/multi-vterm-toggle-dwim)))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?i)))

(use-package shackle
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
     (vterm-mode                      :align below :popup t :select t)
     ("*Process List*"                :align below :popup t :select t)
     ("*Warnings*"                    :align below :popup t :select t)
     ("*dired-check-process output*"  :align below :popup t :select t)
     ("*eshell*"                      :align below :popup t :select t)
     (help-mode                       :align right  :popup t :select t :size 82)
     (helpful-mode                    :align right :popup t :select t :size 82)))
  :init
   (shackle-mode))

(use-package popper
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
	  "\\*Warnings*\\*"
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
  :init
  (solaire-global-mode))

;; projects
(use-package otpp
  :after project
  :bind (("C-x t D" . otpp-detach-buffer-to-tab)
         ("C-x t C" . otpp-change-tab-root-dir)
         ("C-x t P" . otpp-prefix))
  :custom
  (otpp-project-aware-commands-regexp (rx (seq bol (or "project-" "+project-" "projection-"))))
  :init
  (otpp-mode 1)
  (otpp-override-mode 1))

(use-package compile-multi
  :bind (("<f9>" . compile-multi)))

(use-package compile-multi-embark
  :after embark
  :init
  (compile-multi-embark-mode 1))

(use-package consult-compile-multi
  :after consult
  :init
  (consult-compile-multi-mode 1))

(use-package compile-multi-nerd-icons
  :after compile-multi
  :demand t)

(use-package projection
  :hook (ibuffer . ibuffer-projection-set-filter-groups)
  :after project
  :demand
  :bind-keymap ("C-x P" . projection-map)
  :init
  ;; This ensures that `ibuffer-projection-set-filter-groups' takes effect
  (add-hook 'ibuffer-hook (lambda () (run-at-time 0.1 nil (lambda () (call-interactively #'ibuffer-update)))))
  ;; Mark compile commands as safe (customized in ".dir-locals.el")
  (dolist (var '( projection-commands-configure-command projection-commands-build-command
                  projection-commands-test-command projection-commands-run-command
                  projection-commands-package-command projection-commands-install-command))
    (put var 'safe-local-variable #'stringp))
  ;; Enable `projection-hook', adds the possibility to run functions in per-project basis
  (global-projection-hook-mode 1))

;; Projection extension to jump between related files in a project
(use-package projection-find
  :after projection
  :ensure nil
  :config
  ;; Add header/source mapping for Modula-2
  (cl-callf2 append '(("mod" "def") ("def" "mod")) projection-find-other-file-suffix))

;; Projection integration for `compile-multi'
(use-package projection-multi
  :after compile-multi projection
  :bind (:map projection-map ("C" . #'projection-multi-compile)))

;; Integration for `projection-multi' and `embark'
(use-package projection-multi-embark
  :after embark compile-multi projection
  :init
  (projection-multi-embark-setup-command-map))

;; Projection integration for `dape'
(use-package projection-dape
  :after dape projection
  :bind (:map projection-map ("D" . #'projection-dape)))

(use-package helpful
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
  :init
  ;; enable god-mode support
  (which-key-enable-god-mode-support))

(use-package evil
  :init
  ;; required for evil-collection
  (setq evil-want-keybinding nil)
  :custom
  (evil-undo-system 'undo-redo)
  :config
  (defun v3rse/vterm-copy-mode-evil ()
    (if (bound-and-true-p vterm-copy-mode)
	(evil-normal-state)
      ;; because evil-emacs-state doesn't work well with god-mode
      (evil-god-toggle-execute-in-god-off-state)))
  (add-hook 'vterm-copy-mode-hook #'my/vterm-copy-mode-evil)
  (evil-mode 1)
  (evil-set-initial-state 'newsticker-treeview-mode 'emacs)
  (evil-set-initial-state 'newsticker-treeview-list-mode 'emacs)
  (evil-set-initial-state 'newsticker-treeview-item-mode 'emacs)
  (evil-set-initial-state 'kubernetes-mode 'emacs))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-god-toggle
  :after (evil god-mode which-key)
  :vc (evil-god-toggle :url "https://github.com/jam1015/evil-god-toggle.git"
		       :branch "main")
  :init
  (setq evil-god-toggle-persist-visual 'always
        ;; Make god-mode global (applies to all buffers) instead of buffer-local:
        evil-god-toggle-global t)
  :config
  (evil-god-toggle-mode 1)

 ;; enter god mode for one command
 (evil-define-key '(normal insert)
    evil-god-toggle-mode-map
    (kbd "C-;") #'evil-god-toggle-execute-in-god-state)

 ;; use escape in both god modes to normal
 (evil-define-key '(god god-off)
   evil-god-toggle-mode-map
   [escape] (lambda ()
              (interactive)
              (evil-god-toggle-stop-choose-state 'normal)))

 ;; enter god mode for one command
 (evil-define-key '(normal insert)
    evil-god-toggle-mode-map
    (kbd "C-,") #'evil-god-toggle-once)

 ;; flip-flop between on/of god mode in any state
 (evil-define-key '(god god-off)
    evil-god-toggle-mode-map
    (kbd "C-M-;") #'evil-god-toggle-god-toggle)
   
 (setq evil-god-state-cursor     '(box    "IndianRed3")
       evil-god-off-state-cursor '(bar    "IndianRed3")
       evil-normal-state-cursor  '(box "SkyBlue")
       evil-insert-state-cursor  '(bar "SkyBlue")
       evil-visual-state-cursor  '(bar "SkyBlue")))

;; language
(use-package hcl-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-center-content t
	dashboard-items '((recents   . 5)
                          (projects  . 5))
	dashboard-display-icons-p t
	dashboard-icon-type 'nerd-icons
	dashboard-set-heading-icons t
	dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))

;; -- Server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))
