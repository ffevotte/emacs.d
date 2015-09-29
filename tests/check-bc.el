(letf* ((byte-compile-dest-file-function
         (lambda (filename)
           ""
           "/tmp/byte-compile-test.elc"))
        (encountered-errors nil)
        (orig-fun
         (symbol-function 'byte-compile-warn))
        ((symbol-function 'byte-compile-warn)
         (lambda (&rest args)
           (apply orig-fun args)
           (push args encountered-errors))))
  (byte-compile-file "init.el")
  (when encountered-errors
    (message "\nEncountered byte-compilation warnings:")
    (mapc (lambda (args) (apply #'message args))
          encountered-errors)
    (message "")
    (when (version< emacs-version "25.0.0")
      (kill-emacs 1))))
