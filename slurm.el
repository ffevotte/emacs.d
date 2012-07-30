
;;;;;;;;;;;;;;;;
;; slurm mode ;;
;;;;;;;;;;;;;;;;

(defvar slurm-mode-map nil
  "keymap for slurm-mode.")
(if slurm-mode-map ()
  (progn
    (setq slurm-mode-map (make-sparse-keymap))
    (suppress-keymap slurm-mode-map)
    (define-key slurm-mode-map (kbd "j")   'slurm-job-list)
    (define-key slurm-mode-map (kbd "p")   'slurm-partition-list)
    (define-key slurm-mode-map (kbd "i")   'slurm-cluster-info)
    (define-key slurm-mode-map (kbd "g")   'slurm-refresh)
    (define-key slurm-mode-map (kbd "RET") 'slurm-details)
    (define-key slurm-mode-map (kbd "d")   'slurm-details)
    (define-key slurm-mode-map (kbd "k")   'slurm-job-cancel)
    (define-key slurm-mode-map (kbd "u")   'slurm-job-update)
    (define-key slurm-mode-map (kbd "/ u") 'slurm-filter-user)
    (define-key slurm-mode-map (kbd "/ p") 'slurm-filter-partition)
    (define-key slurm-mode-map (kbd "s u") 'slurm-sort-user)
    (define-key slurm-mode-map (kbd "s p") 'slurm-sort-partition)
    (define-key slurm-mode-map (kbd "s d") 'slurm-sort-default)
    (define-key slurm-mode-map (kbd "s c") 'slurm-sort)))

;;;###autoload
(defun slurm ()
  "Open a slurm-mode buffer to manage jobs."
  (interactive)
  (switch-to-buffer (get-buffer-create "*slurm*"))
  (slurm-mode))

(defun slurm-mode ()
  "Major-mode for interacting with slurm.

Views:
  \\[slurm-job-list] - View jobs list.
  \\[slurm-partition-list] - View partitions list.
  \\[slurm-cluster-info] - View cluster information.
  \\[slurm-refresh] - Refresh current view.

Operations on partitions:
  \\[slurm-details] - Show partition details.

Operations on jobs:
  \\[slurm-details] - Show job details.
  \\[slurm-job-cancel] - Kill (cancel) job.
  \\[slurm-job-update] - Update job.

Manipulations of the jobs list:
  \\[slurm-filter-user] - Filter jobs by user name.
  \\[slurm-filter-partition] - Filter jobs by partition.
  \\[slurm-sort-user] - Sort jobs by user name.
  \\[slurm-sort-partition] - Sort jobs by partition.
  \\[slurm-sort-default] - Default jobs sorting order.
  \\[slurm-sort] - Customize jobs sorting order.
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map slurm-mode-map)
  (setq mode-name "Slurm")
  (setq major-mode 'slurm-mode)
  (hl-line-mode 1)
  (setq buffer-read-only t)
  
  (make-local-variable 'slurm-initialized)
  (setq slurm-initialized nil)

  (slurm-filter-user      (getenv "USER"))
  (slurm-filter-partition "")
  (slurm-sort             "")

  (setq slurm-initialized t)

  (make-local-variable 'slurm-command)
  (make-local-variable 'slurm-view)
  (slurm-job-list))



;; Views

(defun slurm-job-list ()
  "Switch to slurm jobs list view."
  (interactive)
  (when (eq major-mode 'slurm-mode)
    (let ((format-switch "-o '%.7i %9P %30j %8u %2t %.10M %.5D %40R'"))
      (setq slurm-command (format "squeue %s %s %s %s" format-switch slurm-user-switch slurm-partition-switch slurm-sort-switch)))
    (setq mode-name "Slurm (jobs list)")
    (setq slurm-view 'slurm-job-list)
    (slurm-refresh)))

(defun slurm-partition-list ()
  "Switch to slurm partitions list view."
  (interactive)
  (when (eq major-mode 'slurm-mode)
    (setq slurm-command "scontrol show partition")
    (setq mode-name "Slurm (partitions list)")
    (setq slurm-view 'slurm-partition-list)
    (slurm-refresh)))

(defun slurm-refresh ()
  "Refresh current slurm view."
  (interactive)
  (when (eq major-mode 'slurm-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (shell-command "date" t)
    (let ((commands (if (listp slurm-command) slurm-command
                      (list slurm-command))))
      (dolist (command commands)
        (goto-char (point-max))(newline 3)
        (let ((pos1  (point)))
          (insert "> " command)
          (add-text-properties pos1 (point) '(face ((:weight bold)))))
        (newline 2)
        (shell-command command t)))
    (delete-trailing-whitespace)
    (goto-char (point-min))(forward-line 6)
    (setq buffer-read-only t)))

(defun slurm-details ()
  "Show details on the current slurm entity (job or partition depending on the context)."
  (interactive)
  (when (eq major-mode 'slurm-mode)
    (if (eq slurm-view 'slurm-job-list)       (slurm-job-details))
    (if (eq slurm-view 'slurm-partition-list) (slurm-partition-details))))



;; Slurm jobs list manipulation

(defun slurm-filter-user (user)
  "Filter slurm jobs by user."
  (interactive (list (read-from-minibuffer "User name (blank for all)? " slurm-user)))
  (when (eq major-mode 'slurm-mode)
    (make-local-variable 'slurm-user)
    (setq slurm-user user)
    (make-local-variable 'slurm-user-switch)
    (setq slurm-user-switch (if (string= slurm-user "") ""
                              (format "-u '%s'" slurm-user)))
    (when slurm-initialized (slurm-job-list))))

(defun slurm-filter-partition (partition)
  "Filter slurm jobs by partition."
  (interactive (list (read-from-minibuffer "Partition name (blank for all)? " slurm-partition)))
  (when (eq major-mode 'slurm-mode)
    (make-local-variable 'slurm-partition)
    (setq slurm-partition partition)
    (make-local-variable 'slurm-partition-switch)
    (setq slurm-partition-switch (if (string= slurm-partition "") ""
                                   (format "-p '%s'" slurm-partition)))
    (when slurm-initialized (slurm-job-list))))

(defun slurm-sort (arg)
  "Set a custom sorting order for slurm jobs.

ARG must be in a form suitable to be passed as a '-S' switch to the squeue command (see `man squeue')."
  (interactive (list (read-from-minibuffer "Sort by (blank for default)? " slurm-sort)))
  (when (eq major-mode 'slurm-mode)
    (make-local-variable 'slurm-sort)
    (setq slurm-sort arg)
    (make-local-variable 'slurm-sort-switch)
    (setq slurm-sort-switch (if (string= slurm-sort "") ""
                              (format "-S '%s'" slurm-sort)))
    (when slurm-initialized (slurm-job-list))))

(defun slurm-sort-user ()
  "Sort slurm jobs by user."
  (interactive)
  (slurm-sort "U"))

(defun slurm-sort-partition ()
  "Sort slurm jobs by partition."
  (interactive)
  (slurm-sort "P"))

(defun slurm-sort-default ()
  "Revert to default slurm jobs sorting order."
  (interactive)
  (slurm-sort ""))



;; Slurm jobs manipulation

(defun slurm-job-id ()
  (beginning-of-line)
  (cond ((eq slurm-view 'slurm-job-list)    (if (search-forward-regexp
                                                 "^[[:space:]]*\\([[:digit:]]+\\)[[:space:]]"
                                                 (line-end-position) t)
                                                (match-string 1)
                                              (error "Could not extract jobid on this line")))
        ((eq slurm-view 'slurm-job-details) slurm-jobid)
        (t                                  (error "Bad context for slurm-job-id"))))

(defun slurm-job-details ()
  (when (eq major-mode 'slurm-mode)
    (when (eq slurm-view 'slurm-job-list)
      (make-local-variable 'slurm-jobid)
      (let ((jobid  (slurm-job-id)))
        (setq slurm-command (format "scontrol show job %s" jobid))
        (setq slurm-jobid   jobid))
      (setq mode-name "Slurm (job details)")
      (setq slurm-view 'slurm-job-details)
      (slurm-refresh))))

(defun slurm-job-cancel ()
  "Kill (cancel) current slurm job."
  (interactive)
  (when (eq major-mode 'slurm-mode)
    (let ((jobid (slurm-job-id)))
      (when (y-or-n-p (format "Really cancel job %s? " jobid))
        (shell-command (format "scancel %s" jobid))
        (slurm-refresh)))))

(defun slurm-job-update ()
  "Update (modify) current slurm job."
  (interactive)
  (when (eq major-mode 'slurm-mode)
    (let ((jobid (slurm-job-id)))
      (switch-to-buffer (get-buffer-create (format "*slurm update job %s*" slurm-jobid)))
      (slurm-update-mode)
      (setq slurm-command (format "scontrol show job '%s'" jobid))
      (slurm-update-refresh))))



;; Slurm partitions manipulation

(defun slurm-partition-id ()
  (backward-paragraph)(forward-line 1)
  (if (search-forward-regexp "^[[:space:]]*PartitionName=\\(.*\\)[[:space:]]*$" (line-end-position))
      (match-string 1)
    (error "Could not extract partition name on this paragraph")))

(defun slurm-partition-details ()
  (when (eq slurm-view 'slurm-partition-list)
    (slurm-cluster-info (slurm-partition-id))))

(defun slurm-cluster-info (partition)
  "Show global information on the current state of the cluster.

If PARTITION is set, only show that partition's state.
If PARTITION is `nil', show stats for the entire cluster."
  (interactive (list nil))
  (when (eq major-mode 'slurm-mode)
    (let ((switch (if partition (format "-p '%s'" partition) "")))
      (setq slurm-command (list 
                           (format "sinfo %s" switch)
                           (format "sinfo -o '%%C' %s" switch))))
    (setq mode-name "Slurm (cluster info)")
    (setq slurm-view 'slurm-cluster-info)
    (slurm-refresh)))




;;;;;;;;;;;;;;;;;;;;;;;
;; slurm-update mode ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defvar slurm-update-mode-map nil
  "keymap for slurm-update-mode.")
(if slurm-update-mode-map ()
  (progn
    (setq slurm-update-mode-map text-mode-map)
    (define-key slurm-update-mode-map (kbd "C-c C-c") 'slurm-update-send)
    (define-key slurm-update-mode-map (kbd "C-c C-q") 'slurm-update-quit)))

(defun slurm-update-mode ()
  "Major-mode for updating slurm entities.

Edit the line you want to update and hit \\[slurm-update-send] to validate your changes.

Key bindings:
  \\[slurm-update-send] - Validate your changes on a line.
  \\[slurm-update-refresh] - Refresh view.
  \\[slurm-update-quit] - Quit this mode.
"
  (interactive)
  (kill-all-local-variables)
  (use-local-map slurm-update-mode-map)
  (setq mode-name "Slurm update")
  (setq major-mode 'slurm-update-mode)
  (make-local-variable 'slurm-command)
  (hl-line-mode 1))

(defun slurm-update-refresh ()
  "Refresh slurm-update buffer."
  (interactive)
  (when (eq major-mode 'slurm-update-mode)
    (let ((old-position (point)))
      (erase-buffer)
      (shell-command slurm-command t)
      (goto-char (point-min))
      (while (re-search-forward "^[[:space:]]+" nil t)
        (replace-match ""))
      (goto-char (point-min))
      (while (search-forward " " nil t)
        (backward-delete-char 1)
        (newline))
      (goto-char (point-min))
      (while (< (forward-line 1) 1)
        (insert "  "))
      (goto-char old-position))))

(defun slurm-update-send ()
  "Validate a parameter change in the slurm-update-buffer."
  (interactive)
  (when (eq major-mode 'slurm-update-mode)
    (let* ((id       (save-excursion (goto-char (point-min))
                                     (buffer-substring (line-beginning-position) (line-end-position))))
           (prop     (buffer-substring (line-beginning-position) (line-end-position)))
           (command  (format "scontrol update %s %s" id prop)))
      (when (eq (shell-command command) 0)
        (slurm-update-refresh)))))

(defun slurm-update-quit ()
  "Quit slurm-update mode."
  (interactive)
  (when (eq major-mode 'slurm-update-mode)
    (kill-buffer)
    (switch-to-buffer "*slurm*")
    (slurm-refresh)))
