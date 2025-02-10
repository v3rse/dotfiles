(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024)) ;; 1MB
(setq large-file-warning-threshold (* 100 1024 1024)) ;; 100MB

(setq make-backup-files nil)

(setq create-lockfiles nil)

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))


;; Basic UI configuration
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")
(set-face-attribute 'default nil :family "Iosevka" :height 125  :weight 'medium :slant 'normal :width 'normal)
(set-face-attribute 'variable-pitch nil :family "Iosevka Etoile" :height 130)

(tooltip-mode 0)
(global-completion-preview-mode 1)
(which-key-mode 1)
;;(tab-bar-mode 0)
;;(desktop-save-mode 1)

;; modeline
(setq mode-line-format (default-value 'mode-line-format))

;; tab bar mode
;;(setq tab-bar-show nil)
;;(add-hook 'emacs-startup-hook #'tab-switcher)

(setq completion-styles '(basic flex)
      completion-auto-select t
      completion-auto-help 'visible
      completions-format 'one-column
      completions-sort 'historical
      completions-max-height 20
      completion-ignore-case t)

(fido-vertical-mode 1)


;; Simple fringe configuration
(modify-all-frames-parameters '((internal-border-width . 24)
                                (left-fringe . 0)
                                (right-fringe . 0)))

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

;; Basic Coding Settings
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
;; Diary
(setq diary-file "~/org/emacs-diary")

;; Org
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
		org-insert-heading-respect-content t))

(setq-default fill-column 90)
(add-hook 'org-mode-hook 'visual-line-mode)

(use-package org-attach
  :after org
  :ensure nil)

(use-package org-tempo
  :after org
  :ensure nil
  :config
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("q" . "quote")))

(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "PROJ(p)" "|" "DONE(d)" "CNCL(c)")))

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
	  ("j" "Journal" entry (file+datetree "journal.org")
	   "* %?\nEntered on %U\n %i\n %a")))
	
(add-to-list 'org-modules 'org-habit)

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

(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)

;; News
(setq newsticker-url-list
      '(
          ("Euronews" "https://www.euronews.com/rss")
          ("Allsides New" "https://www.allsides.com/rss/news")
	  ("arstechnica" "https://feeds.arstechnica.com/arstechnica/index")
          ;;("Sacha Chua" "https://sachachua.com/blog/feed/")
          ("Recurse" "https://blaggregator.recurse.com/atom.xml?token=561d4f124fc342d78c6e25da65dfd69a")
          ("Hacker News" "https://news.ycombinator.com/rss")
          ("Plant Emacs" "https://planet.emacslife.com/atom.xml")
          ("Lobsters" "https://lobste.rs/rss")
;;	  ("Org Mode Woof Feed" "https://tracker.orgmode.org/index.rss")
;;	  ("Joshua Blais" "https://joshblais.com/index.xml")
	)
      )

;; Theming
(use-package emacs
  :config
  (require-theme 'modus-themes)
  
  (setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil
      modus-themes-mixed-fonts t
      modus-themes-custom-auto-reload nil
      modus-themes-prompts '(italic bold)
      modus-themes-custom-auto-reload t
      modus-themes-disable-other-themes t
      modus-themes-variable-pitch-ui nil
      modus-themes-completions
      '((matches . (extrabold))
	(selection . (semibold italic text-also)))
      modus-themes-org-blocks 'gray-background
      modus-themes-headings
      '((0 . (variable-pitch 1.8))
	(1 . (variable-pitch 1.5))
	(2 . (1.3))
	(agenda-date . (1.3))
	(agenda-structure . (variable-pitch light 1.8))
	(t . (1.1)))
      modus-themes-common-palette-overrides
      `(
	(bg-main "#2E3440") (fg-main "#ECEFF4")
	(border-mode-line-active unspecified)
	(border-mode-line-inactive unspecified)
	,@modus-themes-preset-overrides-cooler))
  
  ;; Load a simple theme
  (load-theme 'modus-vivendi-tinted :no-confirm)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))


;; Default Brower
(setq browse-url-browser-function 'eww-browse-url)

