;; scroll compilation buffer until first error
(setq compilation-scroll-output 'first-error)



;; ANSI coloring in compilation buffers
(require 'ansi-color)
(defun ff/ansi-colorize-buffer ()
  (setq buffer-read-only nil)
  (ansi-color-apply-on-region (point-min) (point-max))
  (setq buffer-read-only t))
(add-hook 'compilation-filter-hook 'ff/ansi-colorize-buffer)



;; Handle LaTeX compilation errors and warnings

;; Parse LaTeX output to determine the source file
(defun ff/compilation-error-latex-file ()
  "Analyse the LaTeX output to find the source file in which an error was reported."
  (condition-case nil
      (save-excursion
        (save-match-data
          (let ((found  nil)
                (bound  (point))
                beg
                filename)
            (while (not found)
              ;; Search backward for an opening paren
              (search-backward "(" nil)

              ;; Try to find a matching closing paren
              (condition-case nil
                  (save-excursion
                    (goto-char (scan-sexps (point) 1))

                    (when (or (> (point) bound)         ;; Closing paren after the error message
                              (not (looking-back ")"))) ;; Non-matching closing delimiter
                      (setq found t)))

                ;; Unbalanced expression
                ((error)
                 (setq found t))))

            ;; Extract filename
            (setq beg (1+ (point)))
            (re-search-forward "[[:space:]]" nil)
            (setq filename (buffer-substring beg (- (point) 1)))
            (list filename))))

    ;; Unexpected error
    ((error)
     nil)))

;; Unfill "LaTeX Warning" lines
(defun ff/compilation-LaTeX-filter ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (while (re-search-forward "^LaTeX Warning: " nil t)
      (while (not (re-search-forward "\\.$" (line-end-position) t))
        (end-of-line)
        (delete-char 1)
        (beginning-of-line)))
    (setq buffer-read-only t)))
(add-hook 'compilation-filter-hook 'ff/compilation-LaTeX-filter)

;; Add LaTeX warnings detection to the list
(add-to-list 'compilation-error-regexp-alist 'latex-warning)
(add-to-list 'compilation-error-regexp-alist-alist
             '(latex-warning
               "^LaTeX Warning: .* on input line \\([[:digit:]]+\\)\\.$" ;; Regular expression
               ff/compilation-error-latex-file                           ;; Filename
               1                                                         ;; Line number
               nil                                                       ;; Column number
               1))                                                       ;; Type (warning)
