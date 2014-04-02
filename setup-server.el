(require 'server)

(defvar ff/main-server-name "server")

(defun ff/server-start ()
  "Start an emacs server using an automatically generated name.

If an emacs server is already running, it is restarted."
  (if (and (not (string= server-name ff/main-server-name))
           (boundp 'server-process)
           server-process
           (memq (process-status server-process) '(connect listen open run)))
      ;; There is already an instance running; just restart it
      (server-start)

    ;; Start a new server
    (let ((i       0)
          (max-try 100)
          (ok      nil))
      (while (and (< i max-try)
                  (not ok))
        (setq server-name (format "server%d" i))
        (setq i (1+ i))
        (unless (server-running-p server-name)
          (setq ok t)))
      (if (>= i max-try)
          (display-warning 'ff/server-start
                           "Could not find any unused server name."
                           :warning)
        (message "Starting server with name `%s'." server-name)
        (server-start))))
  (setenv "EMACS_SERVER" server-name))

(defun ff/main-server ()
  (interactive)
  (when (and (boundp 'server-process)
             server-process
             (memq (process-status server-process) '(connect listen open run))
             (not (string= server-name ff/main-server-name)))
    ;; There is already an instance running under a different name; kill it
    (server-force-delete)
    (delete-process server-process))

  (setq server-name ff/main-server-name)
  (message "Starting main emacs server with name `%s'." server-name)
  (server-start)
  (setenv "EMACS_SERVER" server-name))

(provide 'setup-server)