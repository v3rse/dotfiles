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
(tab-bar-mode 1)
(desktop-save-mode 1)

;; tab bar mode
(setq tab-bar-show nil)
(add-hook 'emacs-startup-hook #'tab-switcher)

(setq completion-styles '(basic flex)
      completion-auto-select t
      completion-auto-help 'visible
      completions-format 'one-column
      completions-sort 'historical
      completions-max-height 20
      completion-ignore-case t)

(fido-vertical-mode 1)

;; Load a simple theme
(load-theme 'modus-vivendi-tinted t)

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
                org-default-notes-file "~/org/gtd/inbox.org"
                org-agenda-files '("gtd/inbox.org" "gtd/agenda.org" "gtd/projects.org")
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
        `(("t" "Task" entry (file+headline "gtd/inbox.org" "Tasks")
           ,(string-join '("* TODO %?"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":CATEGORY: Task"
                           ":END:")
                         "\n"))
          ("n" "Note" entry (file+headline "gtd/inbox.org" "Notes")
           ,(string-join '("* %?"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":CATEGORY: Note"
                           ":END:")
                         "\n"))
          ("m" "Meeting" entry (file+headline "gtd/inbox.org" "Meetings")
           ,(string-join '("* %? :MEETING"
                           "<%<%Y-%m-%d %a %H:00>>"
                           ""
                           "/Met with: /")
                         "\n"))
          ("a" "Appointment" entry (file+headline "gtd/inbox.org" "Appointments")
           ,(string-join '("* %? :APPOINTMENT:"
                           ":PROPERTIES:"
                           ":CREATED: %U"
                           ":CATEGORY: Appointment"
                           ":END:")
                         "\n"))))
	
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
                        (org-agenda-files '("gtd/inbox.org"))))
                (todo "NEXT"
                        ((org-agenda-overriding-header "In Progress")
                                (org-agenda-files '("gtd/someday-maybe.org"
                                                "gtd/projects.org"
                                                "gtd/agenda.org"))))
                (todo "PROJ"
                        ((org-agenda-overriding-header "Projects")
                                (org-agenda-files '("gtd/projects.org"))))
                (todo "TODO"
                      ((org-agenda-overriding-header "One-off Tasks")
                       (org-agenda-files '("gtd/agenda.org"))
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
                 (agenda nil
                         ((org-agenda-entry-types '(:deadline))
                          (org-deadline-warning-days 7)
                          (org-agenda-overriding-header "\nDeadlines\n")))
                 (tags "CLOSED>=\"<today>\""
                       ((org-agenda-overriding-header "\nCompleted today\n")))
                 ))))
(setq v3rse/org-refile-target-files '("gtd/agenda.org"
                                      "gtd/projects.org"
                                      "gtd/someday-maybe.org"
                                      "research/notes.org"))


(setq v3rse/org-refile-file-paths
      (let (result)
        (dolist (file v3rse/org-refile-target-files result)
          (push (expand-file-name file org-directory) result))))

(setq org-refile-targets
      '((nil :maxlevel . 9)
        (v3rse/org-refile-file-paths :maxlevel . 9)))

(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)

;; News
(setq newsticker-url-list
      '(
          ("Euronews" "https://www.euronews.com/rss")
          ("Allsides New" "https://www.allsides.com/rss/news")
	  ("arstechnica" "https://feeds.arstechnica.com/arstechnica/index")
          ("Sacha Chua" "https://sachachua.com/blog/feed/")
          ("Recurse" "https://blaggregator.recurse.com/atom.xml?token=561d4f124fc342d78c6e25da65dfd69a")
          ("Hacker News" "https://news.ycombinator.com/rss")
          ("Plant Emacs" "https://planet.emacslife.com/atom.xml")
          ("Lobsters" "https://lobste.rs/rss")
	  ("Org Mode Woof Feed" "https://tracker.orgmode.org/index.rss")
	)
      )
;; Communication


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(command-log-mode company-box corfu counsel denote doom-modeline
		      doom-themes ef-themes elfeed evil-collection
		      exwm flycheck fontaine go-mode helpful htmlize
		      ivy-rich js2-mode lsp-ivy lsp-ui magit
		      marginalia minions modus-themes org-bullets
		      org-modern org-super-agenda paredit prettier-js
		      projectile rainbow-delimiters spacious-padding
		      typescript-mode vc-use-package vertico
		      visual-fill-column web-mode which-key yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

