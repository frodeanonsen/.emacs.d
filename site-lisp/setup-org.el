;;; package --- org-mode configuration
;;;
;;; Commentary:
;;; Org-mode related stuff
;;;
;;; Code:
(require 'use-package)

(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(use-package org-bullets :ensure t)

(use-package org-mode
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture))
  :init (progn
          (setq org-directory "~/git/gtd")
          (setq org-default-notes-file (concat org-directory "/refile.org"))
          (setq org-agenda-files (quote ("~/git/gtd")))

          (setq org-todo-keywords
                (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                        (sequence "SOMEDAY(s)" "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

          ;; (setq org-tag-alist '(("@home" . ?h)
          ;;                       ("@pc" . ?p)
          ;;                       ("@mail" . ?m)
          ;;                       ("@sandnes" . ?s)
          ;;                       ("@out" . ?o) 
          ;;                       ("@reading" . ?r)))

          (setq org-tag-persistent-alist 
                '((:startgroup . nil)
                  ("@home" . ?h) 
                  ("@computer" . ?c)
                  ("@email" . ?e)
                  ("@sandnes" . ?s)
                  ("@out" . ?o)
                  ("@reading" . ?r)
                  ("@work" . ?w)
                  (:startgroup . nil)
                  (:grouptags . nil)
                  ("knowit" . ?n)
                  ("kydonia" . ?k)
                  (:endgroup . nil)
                  (:endgroup . nil)
                  (:startgroup . nil)
                  ("project" . ?p) 
                  ("maybe" . ?m)
                  ("interesting" . ?i)
                  (:endgroup . nil)
                  )
                )

          ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
          (setq org-capture-templates
                (quote (("t" "todo" entry (file (concat org-directory "/refile.org"))
                         "* TODO %?\n%U\n")
                        ("r" "respond" entry (file (concat org-directory "/refile.org"))
                         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                        ("n" "note" entry (file (concat org-directory "/notes.org"))
                         "* %? :note:\n%U\n")
                        ("j" "Journal" entry (file+datetree (concat org-directory "/diary.org"))
                         "* %?\n%U\n" :clock-in t :clock-resume t)
                        ("l" "Reading list" entry (file+datetree (concat org-directory "/reading.org"))
                         "* TODO %?\n%U\n" :clock-in t :clock-resume t)
                        ("w" "org-protocol" entry (file (concat org-directory "/refile.org"))
                         "* TODO Review %c\n%U\n" :immediate-finish t)
                        ("m" "Meeting" entry (file (concat org-directory "/refile.org"))
                         "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                        ("p" "Phone call" entry (file (concat org-directory "/refile.org"))
                         "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                        ("h" "Habit" entry (file (concat org-directory "/refile.org"))
                         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))
                )

          ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
          (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                           (org-agenda-files :maxlevel . 1))))


          ;; Do not dim blocked tasks
          (setq org-agenda-dim-blocked-tasks nil)

          ;; Compact the block agenda view
          ;;(setq org-agenda-compact-blocks t)

          ;; Custom agenda command definitions
          (setq org-agenda-custom-commands
                (quote (
                        ("w" . "Work tasks...")
                        ("wn" "NEXT Knowit"
                         tags-todo "TODO=\"NEXT\"&knowit"
                         ((org-agenda-overriding-header "Next Knowit tasks")
                          (org-tags-match-list-sublevels 9)))
                        ("wk" "NEXT Kydonia"
                         tags-todo "TODO=\"NEXT\"&kydonia"
                         ((org-agenda-overriding-header "Next Kydonia tasks")
                          (org-tags-match-list-sublevels 9)))
                        ("N" "Notes" tags "note"
                         ((org-agenda-overriding-header "Notes")
                          (org-tags-match-list-sublevels t)))
                        ("h" "Habits" tags-todo "STYLE=\"habit\""
                         ((org-agenda-overriding-header "Habits")
                          (org-agenda-sorting-strategy
                           '(todo-state-down effort-up category-keep))))
                        (" " "Agenda"
                         ((agenda "" nil)
                          (tags "REFILE"
                                ((org-agenda-overriding-header "Tasks to Refile")
                                 (org-tags-match-list-sublevels nil)))
                          (tags-todo "-CANCELLED/!"
                                     ((org-agenda-overriding-header "Stuck Projects")
                                      (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                                      (org-agenda-sorting-strategy
                                       '(category-keep))))
                          (tags-todo "-HOLD-CANCELLED/!"
                                     ((org-agenda-overriding-header "Projects")
                                      (org-agenda-skip-function 'bh/skip-non-projects)
                                      (org-tags-match-list-sublevels 'indented)
                                      (org-agenda-sorting-strategy
                                       '(category-keep))))
                          (tags-todo "-CANCELLED/!NEXT"
                                     ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                                ""
                                                                              " (including WAITING and SCHEDULED tasks)")))
                                      (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                                      (org-tags-match-list-sublevels t)
                                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                      (org-agenda-sorting-strategy
                                       '(todo-state-down effort-up category-keep))))
                          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                     ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                                ""
                                                                              " (including WAITING and SCHEDULED tasks)")))
                                      (org-agenda-skip-function 'bh/skip-non-project-tasks)
                                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                      (org-agenda-sorting-strategy
                                       '(category-keep))))
                          (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                                     ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                                ""
                                                                              " (including WAITING and SCHEDULED tasks)")))
                                      (org-agenda-skip-function 'bh/skip-project-tasks)
                                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                                      (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                                      (org-agenda-sorting-strategy
                                       '(category-keep))))
                          (tags-todo "-CANCELLED+WAITING|HOLD/!"
                                     ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                            (if bh/hide-scheduled-and-waiting-next-tasks
                                                                                ""
                                                                              " (including WAITING and SCHEDULED tasks)")))
                                      (org-agenda-skip-function 'bh/skip-non-tasks)
                                      (org-tags-match-list-sublevels nil)
                                      (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                                      (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                          (tags "-REFILE/"
                                ((org-agenda-overriding-header "Tasks to Archive")
                                 (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                                 (org-tags-match-list-sublevels nil))))
                         nil)))
                )
          ))

(provide 'setup-org)
;;; setup-org.el ends here
