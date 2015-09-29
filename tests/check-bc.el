(letf* ((byte-compile-dest-file-function
         (lambda (filename)
           ""
           "/tmp/byte-compile-test.elc"))
        (encountered-error nil)
        (orig-fun
         (symbol-function 'byte-compile-warn))
        ((symbol-function 'byte-compile-warn)
         (lambda (&rest args)
           (apply orig-fun args)
           (setq encountered-error t))))
  (byte-compile-file "init.el")
  (when encountered-error
    (kill-emacs 1)))
