;;; org-config.el -*- lexical-binding: t; -*-

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program  "google-chrome-stable")
;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled


(setq-default org-enforce-todo-dependencies t)

  (setq org-todo-keywords
        '((sequence
           "TODO(t)"  ; A task that needs doing & is ready to do
           "NEXT(t)"  ; The nex task in to perform in the project
           "PROJ(p)"  ; A project, which usually contains other tasks
           "WAIT(w@)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))
;; I don't wan't the keywords in my exports by default
(setq-default org-export-with-todo-keywords nil)

;; hide all the  * / _ chars
(setq org-hide-emphasis-markers t)


(defun my/set-specific-faces-org ()
  (set-face-attribute 'org-code nil
                      :inherit '(shadow fixed-pitch))
  ;; Without indentation the headlines need to be different to be visible
  (set-face-attribute 'org-level-1 nil
                      :height 1.25
                      :foreground "#BEA4DB")
  (set-face-attribute 'org-level-2 nil
                      :height 1.15
                      :foreground "#A382FF"
                      :slant 'italic)
  (set-face-attribute 'org-level-3 nil
                      :height 1.1
                      :foreground "#5E65CC"
                      :slant 'italic)
  (set-face-attribute 'org-level-4 nil
                      :height 1.05
                      :foreground "#ABABFF")
  (set-face-attribute 'org-level-5 nil
                      :foreground "#2843FB")
  (set-face-attribute 'org-date nil
                      :foreground "#ECBE7B"
                      :height 0.8)
  (set-face-attribute 'org-document-title nil
                      :foreground "DarkOrange3"
                      :height 1.3)
  (set-face-attribute 'org-ellipsis nil
                      :foreground "#4f747a" :underline nil)
  (set-face-attribute 'variable-pitch nil
                      :family "Roboto Slab" :height 1.2))

(defun my/set-keyword-faces-org ()
  (mapc (lambda (pair) (push pair prettify-symbols-alist))
        '(;; Syntax
          ("#+begin_quote" . "“")
          ("#+end_quote" . "”")))
  (prettify-symbols-mode +1)
  (org-superstar-mode +1)
  )


(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-block-separator #x2501
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t)
(with-eval-after-load 'org-journal
  (define-key org-journal-mode-map (kbd "<C-tab>") 'yas-expand))
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))
(setq org-agenda-deadline-faces
      '((1.0001 . org-warning)              ; due yesterday or before
        (0.0    . org-upcoming-deadline)))  ; due today or later

(setq-default org-icalendar-include-todo t)
(setq org-combined-agenda-icalendar-file "~/org/calendar.ics")
(setq org-icalendar-combined-name "Hugo Org")
(setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(todo-due event-if-todo event-if-not-todo))
(setq org-icalendar-timezone "Europe/Paris")
(setq org-icalendar-store-UID t)
(setq org-icalendar-alarm-time 30)
(setq french-holiday
      '((holiday-fixed 1 1 "Jour de l'an")
        (holiday-fixed 5 8 "Victoire 45")
        (holiday-fixed 7 14 "Fête nationale")
        (holiday-fixed 8 15 "Assomption")
        (holiday-fixed 11 1 "Toussaint")
        (holiday-fixed 11 11 "Armistice 18")
        (holiday-easter-etc 1 "Lundi de Pâques")
        (holiday-easter-etc 39 "Ascension")
        (holiday-easter-etc 50 "Lundi de Pentecôte")
        (holiday-fixed 1 6 "Épiphanie")
        (holiday-fixed 2 2 "Chandeleur")
        (holiday-fixed 2 14 "Saint Valentin")
        (holiday-fixed 5 1 "Fête du travail")
        (holiday-fixed 5 8 "Commémoration de la capitulation de l'Allemagne en 1945")
        (holiday-fixed 6 21 "Fête de la musique")
        (holiday-fixed 11 2 "Commémoration des fidèles défunts")
        (holiday-fixed 12 25 "Noël")
        ;; fêtes à date variable
        (holiday-easter-etc 0 "Pâques")
        (holiday-easter-etc 49 "Pentecôte")
        (holiday-easter-etc -47 "Mardi gras")
        (holiday-float 6 0 3 "Fête des pères") ;; troisième dimanche de juin
        ;; Fête des mères
        (holiday-sexp
         '(if (equal
               ;; Pentecôte
               (holiday-easter-etc 49)
               ;; Dernier dimanche de mai
               (holiday-float 5 0 -1 nil))
              ;; -> Premier dimanche de juin si coïncidence
              (car (car (holiday-float 6 0 1 nil)))
            ;; -> Dernier dimanche de mai sinon
            (car (car (holiday-float 5 0 -1 nil))))
         "Fête des mères")))
(setq calendar-date-style 'european
      holiday-other-holidays french-holiday
      calendar-mark-holidays-flag t
      calendar-week-start-day 1
      calendar-mark-diary-entries-flag nil)

(defun my/style-org-agenda()
  (set-face-attribute 'org-agenda-date nil :height 1.1)
  (set-face-attribute 'org-agenda-date-today nil :height 1.1 :slant 'italic)
  (set-face-attribute 'org-agenda-date-weekend nil :height 1.1))

(add-hook 'org-agenda-mode-hook 'my/style-org-agenda)

(setq org-agenda-breadcrumbs-separator " ❱ "
      org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
      org-agenda-time-grid '((weekly today require-timed)
                             (800 1000 1200 1400 1600 1800 2000)
                             "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
      org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t%b% s")
                                 (todo . " %i %-12:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))

(setq org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
                                               "\n"
                                               (org-agenda-format-date-aligned date))))
(setq org-cycle-separator-lines 2)
(setq org-agenda-category-icon-alist
      `(("Work" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
        ("Personal" ,(list (all-the-icons-material "person")) nil nil :ascent center)
        ("Calendar" ,(list (all-the-icons-faicon "calendar")) nil nil :ascent center)
        ("Reading" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)))

(setq org-agenda-custom-commands
      '(("z" "Hugo view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                          :time-grid t
                          :date today
                          :scheduled today
                          :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(;; Each group has an implicit boolean OR operator between its selectors.
                          (:name "Today"
                           :deadline today
                           :face (:background "black"))
                          (:name "Passed deadline"
                           :and (:deadline past :todo ("TODO" "WAITING" "HOLD" "NEXT"))
                           :face (:background "#7f1b19"))
                          (:name "Work important"
                           :and (:priority>= "B" :category "Work" :todo ("TODO" "NEXT")))
                          (:name "Work other"
                           :and (:category "Work" :todo ("TODO" "NEXT")))
                          (:name "Important"
                           :priority "A")
                          (:priority<= "B"
                           ;; Show this section after "Today" and "Important", because
                           ;; their order is unspecified, defaulting to 0. Sections
                           ;; are displayed lowest-number-first.
                           :order 1)
                          (:name "Waiting"
                           :todo "WAIT"
                           :order 9)
                          (:name "On hold"
                           :todo "HOLD"
                           :order 10)))))))))
(add-hook 'org-agenda-mode-hook 'org-super-agenda-mode)

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook
          (lambda ()
            (setq fill-column 80)
            (define-key org-mode-map (kbd "s-i") 'org-clock-in)
            (define-key org-mode-map (kbd "s-o") 'org-clock-out)
            (define-key org-mode-map (kbd "s-d") 'org-todo)
            (define-key org-mode-map (kbd "M-+") 'text-scale-increase)
            (define-key org-mode-map (kbd "M-°") 'text-scale-decrease)
            (define-key org-mode-map (kbd "C-c \" \"")
                        (lambda () (interactive) (org-zotxt-insert-reference-link '(4))))))

(defun org-journal-save-entry-and-exit()
  "Simple convenience function.
      Saves the buffer of the current day's entry and kills the window
      Similar to org-capture like behavior"
  (interactive)
  (save-buffer)
  (kill-buffer-and-window))

(add-hook 'org-journal-mode-hook
          (lambda ()
            (define-key org-journal-mode-map
                        (kbd "C-x C-s") 'org-journal-save-entry-and-exit)))

(setq org-capture-templates
      '(("n" "Notes" entry
         (file "~/org/inbox.org") "* %^{Description} %^g\n Added: %U\n%?")
        ("m" "Meeting notes" entry
         (file "~/org/meetings.org") "* TODO %^{Title} %t\n- %?")
        ("t" "TODO" entry
         (file "~/org/inbox.org") "* TODO %^{Title}")
        ("e" "Event" entry
         (file "~/org/calendar.org") "* %^{Is it a todo?||TODO |NEXT }%^{Title}\n%^t\n%?")
        ("w" "Work TODO" entry
         (file "~/org/work.org") "* TODO %^{Title}")))

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)
;; Org-roam config
(with-eval-after-load 'org-roam
  ;; Roam is always one level deep in my org-directory
  (setq org-id-link-to-org-use-id t)
  (setq org-roam-completion-system 'helm)
  (add-to-list 'display-buffer-alist
               '(("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :immediate-finish t
           :if-new (file+head "${slug}.org"
                              "#+TITLE: ${title}\n#+hugo_lastmod: Time-stamp: <>\n\n")
           :unnarrowed t)
          ("t" "temp" plain "%?"
           :if-new(file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+TITLE: ${title}\n#+hugo_lastmod: Time-stamp: <>\n\n")
           :immediate-finish t
           :unnarrowed t)
          ("p" "private" plain "%?"
           :if-new (file+head "${slug}-private.org"
                              "#+TITLE: ${title}\n")
           :immediate-finish t
           :unnarrowed t)))
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (push arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))

  (defun my/org-roam-filter-by-tag (tag-name)
    (lambda (node)
      (member tag-name (org-roam-node-tags node))))
  ;; add inbox)

  (defun my/org-roam-list-notes-by-tag (tag-name)
    (mapcar #'org-roam-node-file
            (seq-filter
             (my/org-roam-filter-by-tag tag-name)
             (org-roam-node-list))))

  (defun my/org-roam-refresh-agenda-list ()
    (interactive)
    (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

  ;; Build the agenda list the first time for the session
  (my/org-roam-refresh-agenda-list)

  (defun my/org-roam-project-finalize-hook ()
    "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
    ;; Remove the hook since it was added temporarily
    (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Add project file to the agenda list if the capture was confirmed
    (unless org-note-abort
      (with-current-buffer (org-capture-get :buffer)
        (add-to-list 'org-agenda-files (buffer-file-name)))))

  (defun my/org-roam-find-project ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Select a project file to open, creating it if necessary
    (org-roam-node-find
     nil
     nil
     (my/org-roam-filter-by-tag "Project")
     nil
     :templates
     '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
        :unnarrowed t))))

  (defun my/org-roam-capture-inbox ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
                       :templates '(("i" "inbox" plain "* %?"
                                     :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

  (defun my/org-roam-capture-task ()
    (interactive)
    ;; Add the project file to the agenda after capture is finished
    (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

    ;; Capture the new task, creating the project file if necessary
    (org-roam-capture- :node (org-roam-node-read
                              nil
                              (my/org-roam-filter-by-tag "Project"))
                       :templates '(("p" "project" plain "** TODO %?"
                                     :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                            "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                            ("Tasks"))))))

  (defun my/org-roam-copy-todo-to-today ()
    (interactive)
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies--capture (current-time) t)
        (setq today-file (buffer-file-name))
        (setq pos (point)))

      ;; Only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos)))))

  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (my/org-roam-copy-todo-to-today))))
  (org-roam-db-autosync-mode)
  )
(setq org-id-extra-files (org-roam--list-files org-roam-directory))

(map! :leader
      :desc "Find roam node" "fn" #'org-roam-node-find)
(map! :leader
      :desc "Roam project" "pr" #'my/org-roam-find-project)
(map! :leader
      (:prefix ("r" . "roam")
       :desc "Open random node"           "a" #'org-roam-node-random
       :desc "Find node"                  "f" #'org-roam-node-find
       :desc "Find ref"                   "F" #'org-roam-ref-find
       :desc "Show graph"                 "g" #'org-roam-graph
       :desc "Insert node"                "i" #'org-roam-node-insert
       :desc "Capture to node"            "n" #'org-roam-capture
       :desc "Toggle roam buffer"         "r" #'org-roam-buffer-toggle
       :desc "Launch roam buffer"         "R" #'org-roam-buffer-display-dedicated
       :desc "Sync database"              "s" #'org-roam-db-sync
       (:prefix ("d" . "by date")
        :desc "Goto previous note"        "b" #'org-roam-dailies-goto-previous-note
        :desc "Goto date"                 "d" #'org-roam-dailies-goto-date
        :desc "Capture date"              "D" #'org-roam-dailies-capture-date
        :desc "Goto next note"            "f" #'org-roam-dailies-goto-next-note
        :desc "Goto tomorrow"             "m" #'org-roam-dailies-goto-tomorrow
        :desc "Capture tomorrow"          "M" #'org-roam-dailies-capture-tomorrow
        :desc "Capture today"             "n" #'org-roam-dailies-capture-today
        :desc "Goto today"                "t" #'org-roam-dailies-goto-today
        :desc "Capture today"             "T" #'org-roam-dailies-capture-today
        :desc "Goto yesterday"            "y" #'org-roam-dailies-goto-yesterday
        :desc "Capture yesterday"         "Y" #'org-roam-dailies-capture-yesterday
        :desc "Find directory"            "-" #'org-roam-dailies-find-directory)))

(defun my/caldav-sync-perso ()
  "Sync my local calendar in ~/org/calendar.org with my remote calendar"
  (interactive)
  (let ((org-caldav-inbox "~/org/cal_inbox.org")
        (org-caldav-calendar-id "org")
        (org-caldav-url "https://cld.hugocisneros.com/remote.php/dav/calendars/ncp/")
        (org-caldav-files '("~/org/calendar.org")))
    (call-interactively 'org-caldav-sync)))

(setq org-journal-dir "~/org/dailies/")
(setq org-journal-enable-encryption nil)

(with-eval-after-load "ispell"
  ;; (after! flyspell-lazy
  ;;   (flyspell-lazy-mode -1))
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US,fr_FR")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,fr_FR"))

(add-hook 'before-save-hook 'time-stamp)

(add-hook 'markdown-mode-hook 'my/buffer-face-mode-variable)
(after! mu4e
  (setq org-contacts-files '("~/org/contacts.org"))
  (require 'org-contacts))
(setq org-return-follows-link t)
(setq org-mobile-directory "~/webdav")
(setq org-journal-dir "/home/willefi/org/daily/")
