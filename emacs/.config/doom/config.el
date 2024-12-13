;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setenv "TZ" "Europe/Berlin")

(if (eq window-system 'mac)
        (add-hook 'doom-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0))))

(add-hook! 'visual-fill-column-mode-hook (setq display-line-numbers-mode -1))
(setq display-line-numbers-type t)

(setq fancy-splash-image (concat doom-private-dir "splash.svg"))

(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)

(add-hook! '+doom-dashboard-mode-hook (hl-line-mode -1))

(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))

(setq doom-font (font-spec :family "Iosevka SS04" :size 15 :weight 'medium)
     doom-variable-pitch-font (font-spec :family "ETBembo" :size 20))

(setq doom-theme 'doom-tomorrow-night)

(setq projectile-project-search-path '("~/src/personal" "~/src/other" "~/src/lab"))

(add-hook 'vterm-mode-hook #'goto-address-mode)

(after! org
        (setq org-directory "~/org/"
                org-default-notes-file "~/org/gtd/inbox.org"
                org-agenda-files '("gtd/inbox.org" "gtd/agenda.org" "gtd/projects.org")
                org-ellipsis " ... "
                org-tags-column -80
                org-log-into-drawer t
                org-hide-emphasis-markers t
                org-agenda-start-day nil
                org-log-done 'time))

(after! org
  (add-to-list 'org-modules 'org-habit))

(after! org
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
                        "\n"))
        ))
)

(after! org
        (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "PROJ(p)" "DONE(d)" "CNCL(c)"))
        org-todo-keyword-faces '(("NEXT" . +org-todo-active)
                                ("HOLD" . +org-todo-onhold)
                                ("CNCL" . +org-todo-cancel)
                                ("PROJ" . +org-todo-project))))

(after! org-agenda
        (add-to-list 'org-agenda-custom-commands
        '("g" "Get Things Done (GTD)"
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

(after! org
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
                (v3rse/org-refile-file-paths :maxlevel . 9))))

(setq deft-directory "~/org"
      deft-extensions '("org")
      deft-recursive t)

(use-package nov
  :config (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(setq browse-url-browser-function 'eww-browse-url
      shr-indentation 2
      shr-width 70)

(set-popup-rule! "^\\*eww\\*" :side 'right :size 0.50)

(use-package! gptel
  :config
  (setq! gptel-default-mode 'org-mode)
  (setq! gptel-default-input-format 'org))

(setq elfeed-feeds
        '(
          ;; News sites (using Ground News on Phone for now)
          ("https://www.euronews.com/rss" news euronews)
          ;; Papers
          ("https://rss.arxiv.org/rss/cs" papers arxiv-cs)
          ;; Blogs
          ("https://frontendmasters.com/blog/feed/" blog frontendmasters)
          ("https://sachachua.com/blog/feed/" blog sachachuaemacs)
          ;; Aggregators
          ("https://blaggregator.recurse.com/atom.xml?token=561d4f124fc342d78c6e25da65dfd69a" agg recurse)
          ("https://news.ycombinator.com/rss" agg hackernews)
          ("https://planet.emacslife.com/atom.xml" agg plantemacs)
          ("https://lobste.rs/rss" agg lobsters)
        )
)

(after! elfeed
  (setq elfeed-search-filter "@1-day-ago +unread"))

(after! circe
  (set-irc-server! "irc.libera.chat"
    `(:tls t
      :port 6697
      :nick "v3rse"
      :sasl-username "v3rse"
      :sasl-password "1amHappy"
      :channels ("#emacs" "#systemcrafters" "#org-mode"))))

(after! gnus
  (setq! gnus-select-method
      '(nntp "news.usenet.farm"
             (nntp-open-connection-function nntp-open-tls-stream)
             (nntp-port-number 563))))

(after! mastodon
  (setq! mastodon-instance-url "https://recurse.social"
      mastodon-active-user "v3rse"))

(defvar-local v3rse/reading nil
    "This is set if reading visuals are active")

(defun v3rse/activate-reading ()
    "Makes text more book like and readable especially in org mode"
    (progn
        (variable-pitch-mode)
        (visual-fill-column-mode)
        (display-line-numbers-mode -1)
        (setq v3rse/reading t)
        (message "You're reading in style")))

(defun v3rse/deactivate-reading ()
    "Reverts org reading mode changes"
    (progn
        (variable-pitch-mode -1)
        (visual-fill-column-mode -1)
        (display-line-numbers-mode)
        (setq v3rse/reading nil)
        (message "Back to being a nerd")))

(defun v3rse/read ()
  "Makes text more book like and readable especially in org mode"
  (interactive)
  (if v3rse/reading
      (v3rse/deactivate-reading)
    (v3rse/activate-reading)))

(defvar-local bionic-overlays nil
  "The overlays for bionicification in the current buffer.")

(defun bionic-word ()
  "Bionicify the word at point"
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (beg (car bounds))
         (end (cdr bounds))
         (whole-len (- end beg)))
    (cond
     ((>= whole-len 2)
      (let* ((half-len (/ whole-len 2))
             (real-len (if (or (> whole-len 6) (= whole-len 3))
                           (+ half-len 1)
                         half-len))
             (ov (make-overlay beg (+ beg real-len))))
        (overlay-put ov 'face 'bold)
        (push ov bionic-overlays)))
     ((> (- end beg) 1)
      (let ((ov (make-overlay beg (+ beg 1))))
        (overlay-put ov 'face 'bold)
        (push ov bionic-overlays)))
     (t nil))))

(defun bionic-buffer ()
  "Bionicify all the visible parts of the current buffer."
  (interactive)
  (if (not (null bionic-overlays))
      (bionic-debuffer))
  (save-excursion
    (goto-char (point-min))
    (while (not (= (point) (point-max)))
      (if (looking-at "\\w")
          (bionic-word))
      (forward-to-word 1))))

(defun bionic-debuffer ()
  "Undo the bionicification."
  (interactive)
  (dolist (ov bionic-overlays)
    (delete-overlay ov)))
