(require 'org)

;; Modules
(setq org-modules (append org-modules '(org-habit org-bbdb)))

;; Directories and Files

;; braindump directory
(setq org-braindump-directory (expand-file-name "notes/" org-directory))

;; Default notes file
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))

(setq org-agenda-files
      (list
       ;; inbox.org
       (expand-file-name "inbox.org" org-directory)
       ;; projects.org
       (expand-file-name "projects.org" org-directory)
       ;; agenda.org
       (expand-file-name "agenda.org" org-directory)
       ;; notes.org
       (expand-file-name org-default-notes-file)
       ;; individual notes files
       org-braindump-directory))

;; Capture

;; Custom capture templates
(defun org-custom-capture-templates ()
  (format
   "%s"
   (pcase
       (plist-get org-capture-plist :capture-template)
     ("inbox" "* %^{Item Type|TODO|NEXT|DOING|BLOCKED|REVIEW|FEEDBACK|WAITING|DONE|ARCHIVE} %?
<%<%Y-%m-%d %a %H:00>>")
     ("task" "* %^{Task Type|TODO|NEXT|DOING|BLOCKED|REVIEW|FEEDBACK|WAITING|DONE|ARCHIVE} %?
<%<%Y-%m-%d %a %H:00>>")
     ("agenda"
      "* %^{Agenda Type|MEETING|APPOINTMENT|CANCELLED} %?
SCHEDULED: <%<%Y-%m-%d %a %H:00>>")
     ("recurring"
      "* %^{Recurring Agenda Type|MEETING|APPOINTMENT} %?
SCHEDULED: <%<%Y-%m-%d %a %H:00 +1d>>
:PROPERTIES:
:LOG_INTO_DRAWER: LOGBOOK
:END:
:LOGBOOK:
:END:")
     ("routine"
      "* %^{Routine Type|TODO|NEXT} %?
SCHEDULED: <%<%Y-%m-%d %a %H:00 +1d>>
:PROPERTIES:
:LOG_INTO_DRAWER: LOGBOOK
:END:
:LOGBOOK:
:END:")
     ("habit"
      "* %^{Habit Type|TODO|NEXT} %?
SCHEDULED: <%<%Y-%m-%d %a %H:00 .+2d/4d>>
:PROPERTIES:
:STYLE:           habit
:LOG_INTO_DRAWER: LOGBOOK
:END:
:LOGBOOK:
:END:")
     ("note" "* %^{Note Type||NOTE|TITLE|REFERENCE|SUBJECT} %?\t\t%^G
<%<%Y-%m-%d %a %H:00>>")
     ("dump" "* %^{Note Type||NOTE|TITLE|REFERENCE|SUBJECT} %?\t\t%^G
<%<%Y-%m-%d %a %H:00>>")
     ("dumplink" "* %^{Note Type||NOTE|TITLE|REFERENCE|SUBJECT} %?\t\t%^G
:PROPERTIES:
:LOCATION: %l
:END:
<%<%Y-%m-%d %a %H:00>>"))))

;; :COLUMNS: %(format \"%%25ITEM %%TODO %%3PRIORITY %%TAGS %%LOCATION\")
;; org-mode capture templates
(setq org-capture-templates
      (append
       '(;; inbox
         ("i" "Inbox" entry (file "inbox.org")
          #'org-custom-capture-templates
          :capture-template "inbox"
          :jump-to-captured t)
         ;; task
         ("T" "One Step Task" entry
          (file+olp "projects.org" "One Step Tasks")
          #'org-custom-capture-templates
          :capture-template "task"
          :jump-to-captured t)
         ;; agenda
         ("m" "Meeting" entry
          (file+olp+datetree "agenda.org" "Meeting")
          #'org-custom-capture-templates
          :capture-template "agenda"
          :jump-to-captured t
          :time-prompt t)
         ;; recurring agenda
         ("r" "Recurring Meeting" entry
          (file+olp+datetree "agenda.org" "Recurring")
          #'org-custom-capture-templates
          :capture-template "recurring"
          :jump-to-captured t
          :time-prompt t)
         ;; routine task
         ("R" "Routine" entry
          (file+olp "projects.org" "Recurring" "Routine")
          #'org-custom-capture-templates
          :capture-template "routine"
          :jump-to-captured t
          :time-prompt t)
         ;; habit
         ("h" "Habit" entry
          (file+olp "projects.org" "Recurring" "Habit")
          #'org-custom-capture-templates
          :capture-template "habit"
          :jump-to-captured t
          :time-prompt t)
         ;; notes
         ("n" "Note" entry (file "notes.org")
          #'org-custom-capture-templates
          :capture-template "note"
          :jump-to-captured t)
         ;; brain dump note
         ("b" "New brain dump" entry
          (file org-capture-note-to-file)
          #'org-custom-capture-templates
          :capture-template "dump"
          :jump-to-captured t)
         ;; create a brain dump note with the current link item
         ("N" "New brain dump on point" entry
          (file org-capture-note-to-file)
          #'org-custom-capture-templates
          :capture-template "dumplink"
          :jump-to-captured t)
         ;; search tags
         ("t" "Search all tags" entry
          (file org-capture-search-tags)))
       ;; bbdb contact management
       (when (locate-library "bbdb")
         '(("C" "All contacts" entry
            (file org-capture-bbdb-show))
           ("c" "New contact" entry
            (file org-capture-bbdb-create))
           ("s" "Search contacts" entry
            (file org-capture-bbdb-search))
           ))))

;; Capture notes to a new org file
(defun org-capture-note-to-file ()
  "Create a new org file in on capture"
  (interactive)
  (let ((name (read-string "Filename: ")))
    (expand-file-name
     (format "%s.org" name)
     org-braindump-directory)))

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

;; Key bindings
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c i") 'org-capture-inbox)
(define-key global-map (kbd "C-c T") 'org-capture-task)
(define-key global-map (kbd "C-c b") 'org-capture-braindump)
(define-key global-map (kbd "C-c N") 'org-capture-braindump-at-point)
(define-key global-map (kbd "C-c t") 'org-capture-search-tags)

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
         (org-capture nil "C"))

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
        (sequence "NOTE(n)" "|" "TITLE(t)" "REFERENCE(r)" "SUBJECT(s)")))

;; For clearer view of each states
(setq org-todo-keyword-faces
      '(("TODO" . "systemTealColor")
        ("NEXT" . "salmon")
        ("DOING" . "gold")
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
