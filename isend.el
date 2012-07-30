(define-minor-mode isend-mode
  "Toggle ISend (Interactive Send) mode\\<isend-mode-map>.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When ISend mode is enabled, you can associate the current buffer with
another buffer using `isend-associate' and then send lines to the
associated buffer using \\[isend-send] (`isend-send').

This might be useful to send commands to an interpreter in a terminal
buffer (such as `ansi-term' or `eshell')

\\{isend-mode-map}"
  :init-value nil
  :lighter    " Isend"
  :keymap     '(([C-return] . isend-send)))

(defvar isend-command-buffer nil
  "Buffer to which lines will be sent using `isend-send'.")
(make-variable-buffer-local 'isend-command-buffer)

;;;###autoload
(defun isend-associate (buffername)
 "Set the buffer to which commands will be sent using `isend-send'.
This should usually be something like '*ansi-term*' or '*terminal*'."
 (interactive "b")
 (setq isend-command-buffer buffername)
 (isend-mode 1))

(defun isend-send ()
 "Send the current line to a terminal.
Use `send-command-setbuffer' to set the associated terminal
buffer. If the region is active, all lines spanned by it are
sent."
 (interactive)
 (when (not (boundp 'isend-command-buffer))
   (error "No associated terminal buffer. You should run `isend-associate'"))
 (let ((begin (point))
       (end   (point)))
   (when (use-region-p)
     (setq begin (region-beginning)
           end   (- (region-end) 1)))
   (goto-char begin)
   (setq begin (line-beginning-position))
   (goto-char end)
   (setq end (line-end-position))
   (let ((command (buffer-substring begin end)))
     (with-current-buffer isend-command-buffer
       (goto-char (point-max))
       (insert command)
       (cond ((eq major-mode 'term-mode)(term-send-input))
             (t (funcall (key-binding (kbd "RET")))))))
   (goto-char (line-end-position))
   (when (search-forward-regexp "." nil t)
     (goto-char (line-beginning-position)))))
