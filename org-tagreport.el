(setq org-tagreport-tags  '(("p_sf"."SF")
                            ("p_sc"."SC") 
                            ("p_hp-p_sf-p_sc-p_non"."hors projet")
                            ("p_non"."non imputable") 
                            ("-p_sf-p_sc-p_hp-p_non"."*oublis*")))

(defvar org-tagreport-tags nil
  "alist of tags to use for the tag report, associated with titles.")

(define-derived-mode org-tagreport-mode org-mode "Org tagreport"
  "This mode allows viewing tag-sorted clocking information from
org-mode agenda files.

In addition to usual org-mode keybindings, the following specific
commands are available:
 d    switch to (d)ay view
 w    switch to (w)eek view
 m    switch to (m)onth view
 y    switch to (y)ear view
 f    go (f)orward in time
 b    go (b)ackward in time
 t    (t)oggle detailed display of clocked tasks
 g    update display
 RET  follow link
 h,?  (h)elp
"
  ;; Define keymap
  ;;(suppress-keymap org-tagreport-mode-map)
  (define-key org-tagreport-mode-map (kbd "d") 'org-tagreport-day)
  (define-key org-tagreport-mode-map (kbd "w") 'org-tagreport-week)
  (define-key org-tagreport-mode-map (kbd "m") 'org-tagreport-month)
  (define-key org-tagreport-mode-map (kbd "y") 'org-tagreport-year)
  (define-key org-tagreport-mode-map (kbd "f") 'org-tagreport-forward)
  (define-key org-tagreport-mode-map (kbd "b") 'org-tagreport-backward)
  (define-key org-tagreport-mode-map (kbd "t") 'org-tagreport-toggle-details)
  (define-key org-tagreport-mode-map (kbd "g") 'org-tagreport-update)
  (define-key org-tagreport-mode-map (kbd "<return>") 'org-open-at-point)
  (define-key org-tagreport-mode-map (kbd "h") 'describe-mode)
  (define-key org-tagreport-mode-map (kbd "?") 'describe-mode)

  ;; Define local variables
  (set (make-local-variable 'org-tagreport-start)   10352)
  (set (make-local-variable 'org-tagreport-view)    'week)
  (set (make-local-variable 'org-tagreport-details) nil)
  (org-tagreport-update-start))

(add-hook 'org-tagreport-mode-hook 'org-tagreport-init)
(defun org-tagreport-init ()
  (setq buffer-read-only t)  ;; Read-only buffer
  (flyspell-mode 0)          ;; Disable spell checking
  (auto-fill-mode 0)         ;; Disable auto-filling
  (org-tagreport-update))

(defun org-tagreport-clocked-time (params)
  "Returns an alist of clocked time in agenda files for each tag"
  (let ((files (org-agenda-files t))
        (total 0)
        file)
    (while (setq file (pop files))
      (with-current-buffer (find-buffer-visiting file)
        (let* ((table-data (org-clock-get-table-data "file" params)))
          (setq total (+ total (nth 1 table-data))))))
    total))

;;;###autoload
(defun org-tagreport ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*Org tagreport*"))
  (org-tagreport-mode))

(defun org-tagreport-update ()
  (interactive)
  (toggle-read-only 0)
  (erase-buffer)

  (let* ((tstart org-tagreport-start)
         (tend   (+ tstart (org-tagreport-span)))
         (tstart (format-time-string "%Y-%m-%d" (org-time-from-absolute tstart)))
         (tend   (format-time-string "%Y-%m-%d" (org-time-from-absolute tend)))
         (params (list :tstart tstart :tend tend)))
    (insert (format "Clock report by tag for %s" (org-tagreport-title)))(newline)(newline)
    (insert "| project name | time |")(newline)
    (insert "|--")
    (dolist (tag org-tagreport-tags)
      (let* ((tag-name  (car tag))
             (tag-title (cdr tag))
             (params    (append (list :tags tag-name) params))
             (time      (progn 
                          (message tag-name)
                          (org-tagreport-clocked-time params))))
        (when (> time 0)
          (newline)
          (insert "|" tag-title "|" (org-minutes-to-hh:mm-string time) "|")
          (when org-tagreport-details
            (save-excursion
              (goto-char (point-max))
              (newline)
              (insert (format "* %s" tag-title))(newline-and-indent)
              (insert "#+BEGIN: clocktable :maxlevel 2 :scope agenda :link t")
              (print params (current-buffer))(join-line)(join-line)
              (end-of-line)(newline-and-indent)
              (insert "#+END:")(newline)))))))
  (org-table-align)(end-of-line)(newline)
  (when org-tagreport-details
    (save-excursion
      (while (search-forward "(" nil t)
        (replace-match "" nil t)))
    (save-excursion
      (while (search-forward ")" nil t)
        (replace-match "" nil t)))
    (save-excursion ;; Can't figure out why org-update-all-dblocks doesn't work
      (while (search-forward "#+BEGIN:" nil t)
        (beginning-of-line)
        (org-update-dblock)
        (forward-line 1)))
    (save-excursion
      (while (search-forward "*0:00*" nil t)
        (beginning-of-line)
        (forward-line -1)
        (kill-line 2))))
  (toggle-read-only 1))

(defun org-tagreport-span ()
  (cond ((eq org-tagreport-view 'day)   1)
        ((eq org-tagreport-view 'week)  7)
        (t                                      nil)))

(defun org-tagreport-title ()
  (format-time-string
   (cond ((eq org-tagreport-view 'day)   "%A %e %B %Y (W%V)")
         ((eq org-tagreport-view 'week)  "week %V (%A %e %B %Y)")
         (t                                       nil))
   (org-time-from-absolute org-tagreport-start)))

(defun org-tagreport-update-start ()
  (cond ((eq org-tagreport-view 'week)
         (let* ((sd org-tagreport-start)
                (date (format-time-string "%Y-%m-%dT00:00:00" (org-time-from-absolute sd)))
                (day-of-week (nth 6 (decode-time (date-to-time date))))
                (shift (- day-of-week org-agenda-start-on-weekday)))
           (if (< shift 0) (setq shift (+ shift 7)))
           (setq org-tagreport-start (- sd shift))))))

(defun org-tagreport-forward ()
  (interactive)
  (let* ((leap  (org-tagreport-span)))
    (setq org-tagreport-start (+ org-tagreport-start leap)))
  (org-tagreport-update-start)
  (org-tagreport-update))

(defun org-tagreport-backward ()
  (interactive)
  (setq org-tagreport-start (- org-tagreport-start 1))
  (org-tagreport-update-start)
  (org-tagreport-update))

(defun org-tagreport-day ()
  (interactive)
  (setq org-tagreport-view 'day)
  (org-tagreport-update))

(defun org-tagreport-week ()
  (interactive)
  (setq org-tagreport-view 'week)
  (org-tagreport-update-start)
  (org-tagreport-update))
  
(defun org-tagreport-toggle-details ()
  (interactive)
  (if org-tagreport-details
      (setq org-tagreport-details nil)
    (setq org-tagreport-details t))
  (org-tagreport-update))
