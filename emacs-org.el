;;;;;;;;;;;;;;;;;;;;;;;
;; Org-mode settings ;;
;;;;;;;;;;;;;;;;;;;;;;;


;; Key bindings
;;;;;;;;;;;;;;;
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f11>") 'ff/org-clock-in)
(defun ff/org-clock-in ()
  (interactive)
  (org-clock-in '(4)))

;; Remove S-<arrows> bindings, which conflict with windmove
(mapcar (lambda (key)
          (define-key org-mode-map key nil))
        '([S-right] [S-left] [S-up] [S-down]))




;; Customization
;;;;;;;;;;;;;;;;

;;   global behaviour
(setq org-tags-column        -100)
(setq org-agenda-tags-column -100)
(setq org-src-fontify-natively t)

;;   clock
(setq org-clock-history-length 20)
(org-clock-persistence-insinuate)
(setq org-clock-in-resume t)
(setq org-clock-into-drawer t)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-modeline-total 'current)
(setq org-clock-persist 'history)
(setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
(setq org-clock-report-include-clocking-task t)
(setq org-clocktable-defaults
     (quote (:maxlevel 2 :scope file :block nil :tstart nil :tend nil :step nil :stepskip0 nil
                       :fileskip0 nil :tags nil :emphasize nil :link nil :narrow 50! :indent t
                       :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil)))

;;   agenda
(setq org-agenda-start-with-log-mode t)
(setq org-agenda-time-grid (quote (nil "----------------" (800 1000 1200 1400 1600 1800 2000))))
(setq org-agenda-include-diary t)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)
(setq calendar-holidays nil)

;;   todo
(setq org-todo-keywords '((sequence "TODO" "NEXT" "STARTED" "WAIT" "|" "DONE")))
(setq org-todo-keyword-faces '(("TODO"    . org-todo)
                               ("NEXT"    . org-todo)
                               ("STARTED" . org-todo)
                               ("WAIT"    . org-warning)
                               ("DONE"    . org-done)))
;;   capture
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 1) (nil :maxlevel . 2))))
(setq org-capture-templates `(("n" "Note" entry (file, org-default-notes-file)
                               ,(concat "* %? :note:\n"
                                        "  %U\n"
                                        "  %i")
                               :empty-lines 1 :clock-in t :clock-resume t)
                              ("r" "Réunion" entry (file, org-default-notes-file)
                               ,(concat "* TODO [/] Réunion %? :a_com:\n"
                                        "  DEADLINE: %t\n"
                                        "  - [ ] Trouver Date\n"
                                        "  - [ ] Réserver salle\n"
                                        "  - [ ] Réserver matériel\n"
                                        "  - [ ] CR")
                               :empty-lines 1 :clock-in t :clock-resume t)
                              ("t" "Todo" entry (file, org-default-notes-file)
                               ,(concat "* TODO %?\n"
                                        "  %U\n"
                                        "  %i")
                               :empty-lines 1 :clock-in t :clock-resume t)))

(defun ff/note-header ()
  "Insert generic org header lines for notes files"
  (interactive)
  (save-excursion 
    (goto-char (point-min))
    (when (not (looking-at "#"))
      (insert "#+FILETAGS: note")(newline)
      (insert "#+OPTIONS: H:2 num:t toc:nil")(newline)
      (newline))))


;; HTML export
(setq org-export-html-validation-link "")


;; LaTeX export
(require 'org-latex)
(unless (boundp 'org-export-latex-classes)
  (setq org-export-latex-classes nil))
(add-to-list 'org-export-latex-classes
      '("org-cr"
         "\\documentclass[a4paper,11pt]{article}
          \\input{includeCommon/commonHeaders}
          \\usepackage{crm}
          \\input{cr}
         "
         ("\\section{%s}"       . "\\section*{%s}")
         ("\\subsection{%s}"    . "\\subsection*{%s}")
         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
         ("\\paragraph{%s}"     . "\\paragraph*{%s}")
         ("\\subparagraph{%s}"  . "\\subparagraph*{%s}")))




;; The following was taken from 'http://doc.norang.ca/org-mode.html'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'org-mode-hook 'bh/org-mode-hook)
(defun bh/org-mode-hook ()
  ;; Undefine C-c [ and C-c ] since this breaks my
  ;; org-agenda files when directories are include It
  ;; expands the files in the directories individually
  (org-defkey org-mode-map "\C-c["    'undefined)
  (org-defkey org-mode-map "\C-c]"    'undefined))

(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "note"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Notes and Tasks to Refile")
                       (org-agenda-overriding-header "Tasks to Refile")))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)))
                (tags-todo "-WAIT-CANCELLED/!NEXT|STARTED"
                           ((org-agenda-overriding-header "Next Tasks")
                            (org-agenda-skip-function 'bh/skip-projects-and-habits)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED/!-NEXT-STARTED-WAIT"
                           ((org-agenda-overriding-header "Tasks")
                            (org-agenda-skip-function 'bh/skip-projects-and-habits)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-todo-ignore-scheduled t)
                            (org-agenda-todo-ignore-deadlines t)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (todo "WAIT"
                      ((org-agenda-overriding-header "Waiting and Postponed tasks")
                       (org-agenda-skip-function 'bh/skip-projects-and-habits)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks))))
               nil)
              )))

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (let ((has-subtask)
        (subtree-end (save-excursion (org-end-of-subtree t)))
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (forward-line 1)
      (while (and (not has-subtask)
                  (< (point) subtree-end)
                  (re-search-forward "^\*+ " subtree-end t))
        (when (member (org-get-todo-state) org-todo-keywords-1)
          (setq has-subtask t))))
    (and is-a-task has-subtask)))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
         (subtree-end (save-excursion (org-end-of-subtree t)))
         (has-next (save-excursion
                     (forward-line 1)
                     (and (< (point) subtree-end)
                          (re-search-forward "^\\*+ \\(NEXT\\|STARTED\\) " subtree-end t)))))
    (if (and (bh/is-project-p) (not has-next))
        nil ; a stuck project, has subtasks but no next task
      next-headline)))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (bh/is-project-p)
        nil
      subtree-end)))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (cond
     ((bh/is-project-p)
      subtree-end)
;;     ((org-is-habit-p)
;;      subtree-end)
     (t
      nil))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    (cond
     ((bh/is-project-p)
      next-headline)
;;     ((org-is-habit-p)
;;      next-headline)
     (t
      nil))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    ;; Consider only tasks with done todo headings as archivable candidates
    (if (member (org-get-todo-state) org-done-keywords)
        (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
               (daynr (string-to-int (format-time-string "%d" (current-time))))
               (a-month-ago (* 60 60 24 (+ daynr 1)))
               (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
               (this-month (format-time-string "%Y-%m-" (current-time)))
               (subtree-is-current (save-excursion
                                     (forward-line 1)
                                     (and (< (point) subtree-end)
                                          (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
          (if subtree-is-current
              next-headline ; Has a date in this month or last month, skip it
            nil))  ; available to archive
      (or next-headline (point-max)))))  


(setq bh/keep-clock-running nil)

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (save-restriction
    (widen)
    (org-with-point-at (org-id-find bh/organization-task-id 'marker)
      (org-clock-in '(16)))))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at (or parent-task)
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)
