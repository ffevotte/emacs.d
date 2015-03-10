;; * Lisp utilities

(defmacro with-timer (title &rest forms)
  "Run the given FORMS, counting the elapsed time.
A message including the given TITLE and the corresponding elapsed
time is displayed."
  (declare (indent 1))
  (let* ((nowvar   (make-symbol "now"))
         (errorvar (make-symbol "error"))
         (body    `(progn
                     ,@forms
                     (setq ,errorvar nil))))
    `(let ((,nowvar   (current-time))
           (,errorvar t))
       (message "%s..." ,title)
       (unwind-protect ,body
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s...%s (%.3fs)" ,title (if ,errorvar "ERROR" "done") elapsed))))))

(defmacro let-when (name cond &rest body)
  "If COND is non-nil, let-bind it to NAME and do BODY."
  (declare (indent 2))
  `(let ((,name ,cond))
     (when ,name
       ,@body)))


;; * Filesystem hierarchy

(setq user-init-file (or load-file-name
                         (buffer-file-name)))
(setq user-emacs-directory (file-name-directory
                            user-init-file))

(defun ff/emacsd (name)
  "Path to a file named NAME in `user-emacs-directory'."
  (expand-file-name (concat user-emacs-directory name)))


;; ** Configuration files

(defun ff/load-configuration (name)
  "Load the configuration for package NAME.
This configuration is located in a file named `setup-NAME.el`
under `user-emacs-directory'."
  (load-file (ff/emacsd (concat "setup-" name ".el"))))

;; ** Persistency files

(defun ff/variable-file (name)
  "Path to a variable file of given NAME.
Variable files are located in the \"var\" subdirectory of `user-emacs-directory'"
  (expand-file-name (concat user-emacs-directory "var/" name)))

(setq cask-init-file (ff/variable-file "cask-init.el"))
