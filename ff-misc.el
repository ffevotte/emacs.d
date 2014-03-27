;; * Statistics
(defun count-words (start end)
  "Return number of words between START and END."
  (interactive "r")
  (save-excursion
    (let ((n 0))
      (goto-char start)
      (while (< (point) end)
        (if (forward-word 1)
            (setq n (1+ n))))
      n)))

(defun ff/count-lines-region (start end)
  "Print number of lines, words and characters in the region."
  (interactive "r")
  (unless (region-active-p)
    (setq start (point-min)
          end   (point-max)))
  (let ((lines (count-lines start end))
        (words (count-words start end))
        (chars (- end start)))
    (message "%d lines, %d words, %d chars" lines words chars)))


;; * Text manipulation
(defun unfill-paragraph ()
  "Unfill the paragraph at point.

This repeatedly calls `join-line' until the whole paragraph does
not contain hard line breaks any more."
  (interactive)
  (forward-paragraph 1)
  (forward-paragraph -1)
  (while (looking-at paragraph-start)
    (forward-line 1))
  (let ((beg (point)))
    (forward-paragraph 1)
    (backward-char 1)
    (while (> (point) beg)
      (join-line)
      (beginning-of-line))))

(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))


;; * User interface

(defun non-dedicated-window-list (&optional frame minibuf window)
  "Return the list of non-dedicated windows.
Arguments have the same meaning as in `window-list'."
  (let ((l nil))
    (dolist (w (window-list frame minibuf window))
      (unless (window-dedicated-p w)
        (setq l (cons w l))))
    (nreverse l)))

(defun rotate-windows (count)
  "Rotate your windows"
  (interactive "p")
  (let* ((non-dedicated-windows (non-dedicated-window-list))
         (num-windows (length non-dedicated-windows))
         (i 0)
         (step (+ num-windows count)))
    (cond ((not (> num-windows 1))
           (message "You can't rotate a single window!"))
          (t
           (dotimes (counter (- num-windows 1))
             (let* ((next-i (% (+ step i) num-windows))
                    (w1 (elt non-dedicated-windows i))
                    (w2 (elt non-dedicated-windows next-i))

                    (b1 (window-buffer w1))
                    (b2 (window-buffer w2))

                    (s1 (window-start w1))
                    (s2 (window-start w2)))
               (set-window-buffer w1 b2)
               (set-window-buffer w2 b1)
               (set-window-start w1 s2)
               (set-window-start w2 s1)
               (setq i next-i)))))))

(defun rotate-windows-backwards (count)
  (interactive "p")
  (rotate-windows (- count)))

(provide 'ff-misc)