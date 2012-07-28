(defun ff/semantic-auto-completion ()
  "Activate semantic-ia source for auto-completion if available"
  (when (require 'cedet nil 'noerror)
    (when (>= (string-to-number cedet-version) 1.1)
      (semantic-load-enable-code-helpers)
      (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
      (setq semantic-idle-scheduler-idle-time 0.5)
      (require 'semantic-ia))))
