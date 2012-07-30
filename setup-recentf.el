;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recentf settings and improvements ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Arrange for recentf to always merge its list with `recentf-save-file'.
;;   (this is useful when multiple Emacs instances are running simultaneously)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl) ;; provides `copy-list'

(defvar ff/recentf-marker
  (concat temporary-file-directory "recentf-marker")
  "File used as a marker for recentf list merges.")

(unless (file-exists-p ff/recentf-marker)
  ;; The recentf marker file must exist (otherwise it will be cleaned
  ;; by `recentf-cleanup')
  (with-temp-buffer (write-file ff/recentf-marker)))

(defadvice recentf-load-list (after ff/merge activate)
  "Add a marker in front of the recentf list to remember what has to be merged"
  (recentf-push ff/recentf-marker))

(defadvice recentf-save-list (around ff/merge activate)
  "Merge the local recentf-list with that of `recentf-save-file'.

Files more recent than `ff/recentf-marker' in the local list will
be pushed in front of the global list before saving it."
  (if (string= (car recentf-list) ff/recentf-marker)
      (recentf-load-list)
    (let ((new-recentf-list (copy-list recentf-list)))
      (recentf-load-list)
      (ff/recentf-merge new-recentf-list))
    ad-do-it
    (recentf-push ff/recentf-marker)))

(defadvice recentf-cleanup (after ff/merge activate)
  "Arrange for the recentf list to be merged and synchronized periodically after cleanups.

See `recentf-cleanup' and `recentf-auto-cleanup' for details."
  (recentf-save-list))

(defun ff/recentf-merge (list)
  "Merge LIST with `recentf-list'.

Every file in LIST more recent (i.e. before) `ff/recentf-marker'
will be pushed in front of `recentf-list'."
  (when list
    (let ((file (car list)))
      (unless (string= file ff/recentf-marker)
        (ff/recentf-merge (cdr list))
        (recentf-push file)))))


;; Start and configure recentf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'recentf)
(setq recentf-max-saved-items 1000
      recentf-auto-cleanup    60)
(recentf-mode 1)
