(require 'desktop)
(require 'revive)


;; * Entry points

;;;###autoload
(defadvice desktop-save (before save-windows activate)
  "Also save frames/windows configuraton."
  (desktop--buffers-save)
  (desktop--frames-save))

;;;###autoload
(defadvice desktop-change-dir (after restore-windows activate)
  "Also restore frames/windows configuration"
  (when (file-exists-p (desktop--buffers-file))
    (desktop--buffers-load))
  (when (file-exists-p (desktop--frames-file))
    (desktop--frames-load)))


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


;; * Save/restore frames & windows configuration

(defun desktop--frames-file ()
  "Name of the file where frames configuration will be saved."
  (concat desktop-dirname ".emacs-frames"))

(defun desktop--frames-save ()
  "Save frames/windows configuration.
The configuration is persistently stored in `desktop--frames-file'"
  (with-temp-buffer
    (pp (mapcar (lambda (frame)
                  (with-selected-frame frame
                    (current-window-configuration-printable)))
                (frame-list))
        (current-buffer))
    (write-region nil nil (desktop--frames-file))))

(defun desktop--frames-load ()
  "Load frames/windows configuration.
Restore the configuration stored in `desktop--frames-file'.
Frames are created/deleted to match the number of frames stored
in the configuration."
  (let* ((configs      (with-temp-buffer
                         (insert-file-contents-literally (desktop--frames-file))
                         (read (current-buffer))))
         (config-count (length configs))
         (frames       (frame-list))
         (frame-count  (length frames)))
    (while (< frame-count config-count)
      (make-frame)
      (setq frames      (frame-list)
            frame-count (length frames)))
    (select-frame (car frames))
    (other-frame -1)
    (while (> frame-count config-count)
      (delete-frame)
      (setq frames      (frame-list)
            frame-count (length frames)))
    (select-frame (car frames))
    (mapc (lambda (config)
            (restore-window-configuration config)
            (other-frame 1))
          configs)
    (select-frame (car frames))))

(provide 'setup-desktop)
