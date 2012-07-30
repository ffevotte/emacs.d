;;;;;;;;;;;;;;;;;;;;;;
;; IBuffer settings ;;
;;;;;;;;;;;;;;;;;;;;;;

(require 'ibuffer)

;; Global customization
;;;;;;;;;;;;;;;;;;;;;;;

(setq ibuffer-show-empty-filter-groups nil
      ibuffer-marked-char ?✓)

(setq ibuffer-saved-filter-groups
      `(("default"
         ("Terminals" (mode . term-mode))
         ("emacs.d" (filename . ,(expand-file-name "~/.emacs.d/")))
	 ("Help" (or (mode . Man-mode)
                     (mode . woman-mode)
                     (mode . Info-mode)
                     (mode . Help-mode)
                     (mode . help-mode)))
         ("Emacs internal" (or (name . "*Messages*")
                               (name . "*Completions*")
                               (name . "*Helm log*")
                               (name . "*helm recentf*")
                               (name . "*ESS*")
                               (name . "*Compile-Log*"))))))


(add-hook 'ibuffer-mode-hook 'ff/ibuffer-mode-hook)
(defun ff/ibuffer-mode-hook ()
  (ibuffer-switch-to-saved-filter-groups "default")
  (local-set-key (kbd "M-<up>")   'ff/ibuffer-hide-all-filters)
  (local-set-key (kbd "M-<down>") 'ff/ibuffer-show-all-filters)
  (local-set-key (kbd "/ f")      'ff/ibuffer-filter-by-filename))

(defun ff/ibuffer-setup ()
  (add-to-list 'ibuffer-formats
               '(mark modified read-only " "
                      (name 30 30 :left :elide) " "
                      (mode  7  7 :left :elide) " "
                      (filename-and-process 10 -1 :left))))
(eval-after-load "ibuffer" '(ff/ibuffer-setup))


;; helper functions
;;;;;;;;;;;;;;;;;;;

(defun ff/ibuffer-filter-by-filename (&optional path)
  "Add ibuffer filter by filename using current buffer file name as default"
  (interactive (list
                (let ((buf (ibuffer-current-buffer)))
                  (if buf
                      (read-from-minibuffer "Filter by path: " (buffer-file-name buf))
                    nil))))
  (if path
      (ibuffer-filter-by-filename path)
    (call-interactively 'ibuffer-filter-by-filename)))

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


;; Clean automatically created buffers
(defun ff/ibuffer-clean ()
  "Clean automatically created buffers"
  (interactive)
  (ibuffer-unmark-all ?*)
  (ibuffer-mark-by-mode 'help-mode)
  (ibuffer-mark-by-mode 'magit-mode)
  (ibuffer-mark-by-mode 'occur-mode)
  (ibuffer-mark-by-mode 'grep-mode)
  (ibuffer-mark-by-mode 'dired-mode)
  (ibuffer-mark-by-mode 'completion-list-mode)
  (ibuffer-mark-by-mode 'compilation-mode)
  (ibuffer-mark-by-mode 'Man-mode)
  (ibuffer-mark-by-mode 'browse-kill-ring-mode)
  (ibuffer-mark-by-name-regexp "*anything*")
  (ibuffer-mark-by-name-regexp "*ESS*")
  (ibuffer-mark-by-name-regexp "*Shell Command Output*")
  (ibuffer-mark-by-name-regexp "*Compile-Log*")
  (ibuffer-mark-by-name-regexp "*vc-diff*")
  (ibuffer-do-delete))
