;; This file re-uses large parts from the original a2ps-print.el file from Emacs.

(defvar a2ps-switches nil
  "List of extra command-line switches for a2ps when it is invoked.")

(defvar a2ps-command "a2ps"
  "Path to the a2ps command")

;;;###autoload
(defun a2ps-buffer (argp)
  "Print buffer contents as with Unix command `a2ps'.
`a2ps-switches' is a list of extra command-line switches to pass
to a2ps.

With a prefix argument, interactively ask for extra switches."
  (interactive "P")
  (a2ps-region-1 (point-min) (point-max) argp "buffer"))

;;;###autoload
(defun a2ps-region (start end argp)
    "Print region contents as with Unix command `a2ps'.
`a2ps-switches' is a list of extra command-line switches to pass
to a2ps.

With a prefix argument, interactively ask for extra switches."
  (interactive "r\nP")
  (a2ps-region-1 start end argp "region"))

(defun a2ps-region-1 (start end argp type)
  (let* ((buffer    (current-buffer))
         (doc-name  (buffer-name))
         (suffix    (if buffer-file-name
                        (substring buffer-file-name
                                   (string-match ".[^.]*$" buffer-file-name))
                      ""))
         (filename  (make-temp-file "emacs-a2ps." nil suffix))
         (switches  a2ps-switches))
    (when argp
      (setq switches (append switches (split-string (read-string "switches: ")))))
    (find-file filename)
    (insert-buffer-substring buffer start end)
    (setq buffer-file-coding-system 'iso-latin-9-unix)
    (save-buffer)
    (apply 'call-process
           (nconc (list "env" nil "*a2ps*" nil "LANG=C" a2ps-command
                        (concat "--center-title=" doc-name)
                        (concat "--footer=(Emacs " type ")")
                        ;"-o" "/tmp/a2ps-test.ps" ;; Uncomment for debugging purposes
                        filename)
                  switches))
    (kill-buffer)
    (delete-file filename)))
