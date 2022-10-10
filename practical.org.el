(require 'org)

;; Modules
(setq org-modules (append org-modules '(org-habit ol-bbdb)))

;; Directories and Files

;; braindump directory
(setq org-braindump-directory (expand-file-name "notes/" org-directory))

;; notes.org
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))

;; inbox.org
(setq org-default-inbox-file (expand-file-name "inbox.org" org-directory))

;; projects.org
(setq org-default-projects-file (expand-file-name "projects.org" org-directory))

;; agenda.org
(setq org-default-agenda-file (expand-file-name "agenda.org" org-directory))

(setq org-agenda-files
      (list
       (expand-file-name org-default-inbox-file)
       (expand-file-name org-default-projects-file)
       (expand-file-name org-default-agenda-file)
       (expand-file-name org-default-notes-file)
       ;; individual notes files
       org-braindump-directory))

;; Defaults context file
(setq org-default-context-file (expand-file-name "context.txt" org-directory))

;; Custom functions

;; return the current time in 12 hours or 24 hours
(defun org-custom-timestamp-format ()
  (if (bound-and-true-p org-timestamp-12-hours)
      (progn
        (setq calendar-time-display-form '12-hours)
        (format-time-string "%Y-%m-%d %a %l:%M %p" (current-time)))
    (progn
      (setq calendar-time-display-form '24-hours)
      (format-time-string "%Y-%m-%d %a %H:00" (current-time)))))

;; check if non-scheduled items should be hidden
(defun org-custom-inbox-timestamp ()
  (if (bound-and-true-p org-hide-all-non-scheduled-items)
      (progn
        (format "[%s]"
                (org-custom-timestamp-format)))
    (progn
      (format "<%s>"
              (org-custom-timestamp-format)))))

;; return the current time in 12 hours or 24 hours for date/time prompt
(defun org-custom-timestamp-prompt-format ()
  (if (bound-and-true-p org-timestamp-12-hours)
      (progn
        (setq calendar-time-display-form '12-hours)
        (format-time-string "%Y-%m-%d %a %l:%M %p" (org-time-string-to-time (org-read-date nil nil org-read-date-final-answer))))
    (progn
      (setq calendar-time-display-form '24-hours)
      (format-time-string "%Y-%m-%d %a %H:00" (org-time-string-to-time (org-read-date nil nil org-read-date-final-answer))))))

;; check if non-scheduled items should be hidden for date/time prompt
(defun org-custom-inbox-prompt-timestamp ()
  (if (bound-and-true-p org-hide-all-non-scheduled-items)
      (progn
        (format "[%s]"
                (org-custom-timestamp-prompt-format)))
    (progn
      (format "<%s>"
              (org-custom-timestamp-prompt-format)))))

;; source all the custom TAGS from a context file
(defun org-build-context-from-file ()
  (dolist (p (split-string (with-temp-buffer
                             (insert-file-contents org-default-context-file)
                             (buffer-substring-no-properties
                              (point-min)
                              (point-max))) "\n" t))
    (push (list p) org-tag-alist)))

;; load the default context file if exists
(if (file-exists-p org-default-context-file)
    (if (not (bound-and-true-p org-disable-context-file))
        (progn
          (org-build-context-from-file))))

;; setup org-mobile idle timer
(if (bound-and-true-p org-mobile-directory)
    (progn
      ;; mobile sync
      (defvar org-mobile-sync-timer nil)
      (defvar org-mobile-sync-idle-secs (* 60 5))
      (defun org-mobile-sync ()
        (interactive)
        (org-mobile-pull)
        (org-mobile-push))
      (defun org-mobile-sync-enable ()
        "enable mobile org idle sync"
        (interactive)
        (setq org-mobile-sync-timer
              (run-with-idle-timer org-mobile-sync-idle-secs t 'org-mobile-sync)))
                                        ;
      (defun org-mobile-sync-disable ()
        "disable mobile org idle sync"
        (interactive)
        (cancel-timer
         org-mobile-sync-timer))
      (org-mobile-sync-enable)))

;; Capture

;; Custom capture templates
(defun org-custom-capture-templates ()
  (format
   "%s"
   (pcase
       (plist-get org-capture-plist :capture-template)
     ("inbox" "* %^{Item Type|TODO|NEXT|DOING|BLOCKED|REVIEW|FEEDBACK|WAITING|DONE|ARCHIVE} %?
%(org-custom-inbox-timestamp)")
     ("task" "* %^{Task Type|TODO|NEXT|DOING|BLOCKED|REVIEW|FEEDBACK|WAITING|DONE|ARCHIVE} %?
%(org-custom-inbox-timestamp)")
     ("agenda"
      "* %^{Agenda Type|MEETING|APPOINTMENT|CANCELLED} %?
SCHEDULED: <%(org-custom-timestamp-prompt-format)>
:PROPERTIES:
:LOCATION: %^{Address/Location/BBDB Contact}
:END:")
     ("recurring"
      "* %^{Recurring Agenda Type|MEETING|APPOINTMENT} %?
SCHEDULED: <%(org-custom-timestamp-prompt-format) +1d>
:PROPERTIES:
:LOCATION: %^{Address/Location/BBDB Contact}
:LOG_INTO_DRAWER: LOGBOOK
:END:
:LOGBOOK:
:END:")
     ("routine"
      "* %^{Routine Type|TODO|NEXT} %?
SCHEDULED: <%(org-custom-timestamp-prompt-format) +1d>
:PROPERTIES:
:LOG_INTO_DRAWER: LOGBOOK
:END:
:LOGBOOK:
:END:")
     ("habit"
      "* %^{Habit Type|HABIT|GOAL|REFINEMENT|TODO|NEXT} %^{What}
SCHEDULED: <%(org-custom-timestamp-prompt-format) .+2d/4d>
:PROPERTIES:
:STYLE:           habit
:LOCATION: %^{Where}
:LOG_INTO_DRAWER: LOGBOOK
:END:
:LOGBOOK:
:END:")
     ("note" "* %^{Note Type||NOTE|TITLE|REFERENCE|SUBJECT} %?\t\t%^G
%(org-custom-inbox-timestamp)")
     ("dump" "* %^{Note Type||NOTE|TITLE|REFERENCE|SUBJECT} %?\t\t%^G
%(org-custom-inbox-timestamp)")
     ("dumplink" "* %^{Note Type||NOTE|TITLE|REFERENCE|SUBJECT} %?\t\t%^G
:PROPERTIES:
:LOCATION: %l
:END:
%(org-custom-inbox-timestamp)"))))

;; :COLUMNS: %(format \"%%25ITEM %%TODO %%3PRIORITY %%TAGS %%LOCATION\")
;; org-mode capture templates
(setq org-capture-templates
      (append
       '(;; show agenda
         ("a" "Show Agenda Menu" entry
          (file org-agenda))
         ("I" "Show/Edit Inbox Items" entry
          (file org-capture-edit-inbox-file))
         ("P" "Show/Edit Projects Items" entry
          (file org-capture-edit-projects-file))
         ("A" "Show/Edit Agenda Items" entry
          (file org-capture-edit-agenda-file))
         ("o" "Show/Edit Notes" entry
          (file org-capture-edit-notes-file))
         ;; inbox
         ("i" "Create a New Inbox Item" entry (file "inbox.org")
          #'org-custom-capture-templates
          :capture-template "inbox"
          :jump-to-captured t)
         ;; task
         ("T" "Create a One Step Task" entry
          (file+olp "projects.org" "One Step Tasks")
          #'org-custom-capture-templates
          :capture-template "task"
          :jump-to-captured t)
         ;; agenda
         ("m" "Create a New Meeting" entry
          (file+olp+datetree "agenda.org" "Meeting")
          #'org-custom-capture-templates
          :capture-template "agenda"
          :jump-to-captured t
          :time-prompt t)
         ;; recurring agenda
         ("r" "Create a New Recurring Meeting" entry
          (file+olp+datetree "agenda.org" "Recurring")
          #'org-custom-capture-templates
          :capture-template "recurring"
          :jump-to-captured t
          :time-prompt t)
         ;; routine task
         ("R" "Create a New Recurring Routine Task" entry
          (file+olp+datetree "projects.org" "Recurring" "Routine")
          #'org-custom-capture-templates
          :capture-template "routine"
          :jump-to-captured t
          :time-prompt t)
         ;; habit
         ("h" "Create a New Recurring Habit Task" entry
          (file+olp+datetree "projects.org" "Recurring" "Habit")
          #'org-custom-capture-templates
          :capture-template "habit"
          :jump-to-captured t
          :time-prompt t)
         ;; notes
         ("n" "Create a New Note" entry (file "notes.org")
          #'org-custom-capture-templates
          :capture-template "note"
          :jump-to-captured t)
         ;; brain dump note
         ("b" "Create a New Brain Dump" entry
          (file org-capture-note-to-file)
          #'org-custom-capture-templates
          :capture-template "dump"
          :jump-to-captured t)
         ;; create a brain dump note with the current link item
         ("N" "Create a New Brain Dump on Point" entry
          (file org-capture-note-to-file)
          #'org-custom-capture-templates
          :capture-template "dumplink"
          :jump-to-captured t)
         ;; search tags
         ("t" "Search All Tags" entry
          (file org-capture-search-tags)))
       ;; context file
       (when (file-exists-p org-default-context-file)
         (if (not (bound-and-true-p org-disable-context-file))
             '(("E" "Edit Contexts & Tags" entry
                (file org-capture-edit-context-file))
               )))
       ;; bbdb contact management
       (when (locate-library "bbdb")
         '(("B" "Show All Contacts" entry
            (file org-capture-bbdb-show))
           ("c" "Create a New Contact" entry
            (file org-capture-bbdb-create))
           ("s" "Search All Contacts" entry
            (file org-capture-bbdb-search))
           ))
       ))

;; Capture notes to a new org file
(defun org-capture-note-to-file ()
  "Create a new org file in on capture"
  (interactive)
  (let ((name (read-string "Filename: ")))
    (expand-file-name
     (format "%s.org" name)
     org-braindump-directory)))

;; Edit the context file
(defun org-capture-edit-context-file ()
  "Edit the context file"
  (interactive)
  (find-file (expand-file-name
              (format "%s" org-default-context-file))))

;; Edit the inbox.org file
(defun org-capture-edit-inbox-file ()
  "Edit the inbox.org file"
  (interactive)
  (find-file (expand-file-name (format "%s" org-default-inbox-file))))

;; Edit the projects.org file
(defun org-capture-edit-projects-file ()
  "Edit the projects.org file"
  (interactive)
  (find-file (expand-file-name (format "%s" org-default-projects-file))))

;; Edit the agenda.org file
(defun org-capture-edit-agenda-file ()
  "Edit the agenda.org file"
  (interactive)
  (find-file (expand-file-name (format "%s" org-default-agenda-file))))

;; Edit the notes.org file
(defun org-capture-edit-notes-file ()
  "Edit the notes.org file"
  (interactive)
  (find-file (expand-file-name (format "%s" org-default-notes-file))))

;; Key shortcuts functions
(defun org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(defun org-capture-task ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "T"))

(defun org-capture-braindump ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "b"))

(defun org-capture-braindump-at-point ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "N"))

(defun org-capture-search-tags ()
  (interactive)
  (call-interactively 'org-tags-view)
  (org-capture nil "t"))

(defun org-capture-edit-context ()
  (interactive)
  (call-interactively 'org-capture-edit-context-file)
  (org-capture nil "E"))

;; Key bindings
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c i") 'org-capture-inbox)
(define-key global-map (kbd "C-c T") 'org-capture-task)
(define-key global-map (kbd "C-c b") 'org-capture-braindump)
(define-key global-map (kbd "C-c N") 'org-capture-braindump-at-point)
(define-key global-map (kbd "C-c t") 'org-capture-search-tags)
(define-key global-map (kbd "C-c E") 'org-capture-edit-context)

;; BBDBv3 contact management
(cond ((locate-library "bbdb")
       (require 'bbdb)

       ;; BBDB Contact Settings

       ;; Set the bbdb contacts file
       ;; (setq bbdb-file (expand-file-name "contacts.bbdb" org-directory))

       ;; BBDB layout
       (setq bbdb-layout 'full-multi-line)

       ;; Don't verify post-codes
       (setq bbdb-check-postcode nil)

       ;; Turn off US number format
       (setq bbdb-phone-style nil)

       ;; BBDB org-capture functions
       (defun org-capture-bbdb-show ()
         (interactive)
         (call-interactively 'bbdb-display-all-records)
         (org-capture nil "B"))

       (defun org-capture-bbdb-create ()
         (interactive)
         (call-interactively 'bbdb-create)
         (org-capture nil "c"))

       (defun org-capture-bbdb-search ()
         (interactive)
         (call-interactively 'bbdb)
         (org-capture nil "s"))))

;; Settings

;; Log time on DONE
(setq org-log-done 'time)

;; Use full window for org-capture
(add-hook 'org-capture-mode-hook 'delete-other-windows)

;; List of all org-files for refiling
(setq org-agenda-refile-targets-files
      (seq-difference
       org-agenda-files
       (list
        org-default-notes-file
        org-braindump-directory)))

;; Refile

;; Turn off logging on refile
(setq org-log-refile nil)

;; Refile targets on org-agenda-files except notes.org is up to level 9
;; Refile on notes.org is set to 1 level
(setq org-refile-targets
      '((nil :maxlevel . 9)
        (org-agenda-refile-targets-files :maxlevel . 9)
        (org-default-notes-file :maxlevel . 1)))

;; Refile on single step
(setq org-outline-path-complete-in-steps nil)

;; Show full path of file when refiling
(setq org-refile-use-outline-path 'file)

;; Allow creating parent nodes on refile
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; State sequences
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "DOING(o)" "|" "DONE(d)" "ARCHIVE(A)")
        (sequence "REVIEW(r)" "|" "FEEDBACK(f)")
        (sequence "WAITING(w)" "|" "HOLD(h)")
        (sequence "BLOCKED(b)" "|" "CANCELLED(c)" "POSTPONED(P)" "CLOSED(C)")
        (sequence "MEETING(m)" "|" "APPOINTMENT(a)")
        (sequence "HABIT(H)" "|" "GOAL(v)" "REFINEMENT(O)")
        (sequence "NOTE(n)" "|" "TITLE(t)" "REFERENCE(R)" "SUBJECT(s)")))

;; For clearer view of each states
(setq org-todo-keyword-faces
      '(("TODO" . "systemTealColor")
        ("NEXT" . "salmon")
        ("DOING" . "gold")
        ("HABIT" . "gold")
        ("GOAL" . "gold")
        ("REFINEMENT" . "gold")
        ("DONE" . "systemIndigoColor")
        ("CLOSED" . "systemIndigoColor")
        ("ARCHIVE" . "systemGrayColor")
        ("MEETING" . "systemBlueColor")
        ("APPOINTMENT" . "systemBlueColor")
        ("HOLD" . "HotPink")
        ("WAITING" . "HotPink")
        ("CANCELLED" . "systemPinkColor")
        ("POSTPONED" . "systemPinkColor")
        ("BLOCKED" . "systemPinkColor")
        ("REVIEW" . "systemPurpleColor")
        ("FEEDBACK" . "systemPurpleColor")
        ("NOTE" . "wheat")
        ("TITLE" . "wheat")
        ("REFERENCE" . "wheat")
        ("SUBJECT" . "wheat")))
