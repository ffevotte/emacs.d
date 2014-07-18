(require 'desktop)
;;(require 'revive)


;; * Entry points

;;;###autoload
(defadvice desktop-save (before save-windows activate)
  "Also save frames/windows configuraton."
  (desktop--buffers-save)
  ;;(desktop--frames-save)
  )

;;;###autoload
(defadvice desktop-change-dir (after restore-windows activate)
  "Also restore frames/windows configuration"
  (when (file-exists-p (desktop--buffers-file))
    (desktop--buffers-load))
  ;;(when (file-exists-p (desktop--frames-file))
  ;;  (desktop--frames-load))
  )


;; * Save/restore special buffers

(defun desktop--buffers-file ()
  "Name of the file where buffers configuration will be saved."
  (concat desktop-dirname "/.emacs-buffers"))

(defun desktop--buffers-save ()
  (let ((special-buffers
         (apply 'append
                (mapcar (lambda (b)
                    (with-current-buffer b
                     (cond
                      ((eq major-mode 'compilation-mode)
                       `((compilation-buffer
                          ,(buffer-name)
                          ,compilation-arguments
                          ,compilation-directory)))
                      ((eq major-mode 'term-mode)
                       `((term-buffer
                          ,(buffer-name)
                          ,default-directory))))))
                        (buffer-list)))))
    (with-temp-buffer
      (pp special-buffers (current-buffer))
      (write-region nil nil (desktop--buffers-file)))))

(defun desktop--buffers-load ()
  (let ((buffers (with-temp-buffer
                   (insert-file-contents-literally (desktop--buffers-file))
                   (read (current-buffer))))

        (compilation-buffer
         (lambda (buffer-name arguments directory)
           (with-current-buffer (get-buffer-create buffer-name)
             (message "Retrieving compilation buffer: %s" buffer-name)
             (compilation-mode)
             (set (make-local-variable 'compilation-arguments) arguments)
             (set (make-local-variable 'compilation-directory) directory))))

        (term-buffer
         (lambda (buffer-name directory)
           (when (null (get-buffer buffer-name))
             (message "Creating term buffer: %s" buffer-name)
             (let ((default-directory directory))
               (ansi-term "/bin/bash"))))))
    (mapc (lambda (buffer)
            (apply (symbol-value (car buffer)) (cdr buffer)))
          buffers)))

(provide 'setup-desktop)
