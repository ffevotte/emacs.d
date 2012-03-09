;;;;;;;;;;;;;;;;;;;;;;
;; IBuffer settings ;;
;;;;;;;;;;;;;;;;;;;;;;


;; Key bindings
;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'ibuffer-mode-hook 'ff/ibuffer-mode-hook)
(defun ff/ibuffer-mode-hook ()
  (ibuffer-switch-to-saved-filter-groups "default")
  (local-set-key (kbd "M-<up>")   'ff/ibuffer-hide-all-filters)
  (local-set-key (kbd "M-<down>") 'ff/ibuffer-show-all-filters)
  (local-set-key (kbd "/ f")      'ff/ibuffer-filter-by-filename))


;; helper functions
;;;;;;;;;;;;;;;;;;;

(defun ff/ibuffer-filter-by-filename (path)
  "Add ibuffer filter by filename using current buffer file name as default"
  (interactive (list
                (let* ((buf  (ibuffer-current-buffer t))
                       (name (buffer-file-name buf)))
                  (read-from-minibuffer "Filter by path: " name))))
  (ibuffer-filter-by-filename path))

(defun ff/ibuffer-hide-all-filters ()
  "Hide all ibuffer filter groups"
  (interactive)
  (save-excursion
    (if (not (eq major-mode 'ibuffer-mode))
        nil
      (progn
        (goto-char 0)
        (setq prev-point 0)
        (while (< prev-point (point))
          (setq prev-point (point))
          (ibuffer-forward-filter-group)
          (ff/ibuffer-hide-filter-group (point)))))))  

(defun ff/ibuffer-show-all-filters ()
  "Show all ibuffer filter groups"
  (interactive)
  (save-excursion
    (if (not (eq major-mode 'ibuffer-mode))
        nil
      (progn
        (goto-char 0)
        (setq prev-point 0)
        (while (< prev-point (point))
          (setq prev-point (point))
          (ibuffer-forward-filter-group)
          (ff/ibuffer-show-filter-group (point)))))))

(defun ff/ibuffer-hide-filter-group (posn)
  "Hide current filter group"
  (let ((name (get-text-property posn 'ibuffer-filter-group-name)))
    (unless (stringp name)
      (error "No filtering group name present"))
    (if (member name ibuffer-hidden-filter-groups)
        nil
      (push name ibuffer-hidden-filter-groups))
    (ibuffer-update nil t)))

(defun ff/ibuffer-show-filter-group (posn)
  "Show current filter-group"
  (let ((name (get-text-property posn 'ibuffer-filter-group-name)))
    (unless (stringp name)
      (error "No filtering group name present"))
    (if (member name ibuffer-hidden-filter-groups)
        (setq ibuffer-hidden-filter-groups
              (delete name ibuffer-hidden-filter-groups)))
    (ibuffer-update nil t)))

;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ff/ibuffer-hide-all-filters)
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)
