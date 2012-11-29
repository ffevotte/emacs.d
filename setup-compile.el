(setq compilation-error-regexp-alist nil)
(setq compilation-error-regexp-alist-alist nil)

;; ANSI coloring in compilation buffers
(require 'ansi-color)
(defun ff/ansi-colorize-buffer ()
  (setq buffer-read-only nil)
  (ansi-color-apply-on-region (point-min) (point-max))
  (setq buffer-read-only t))
(add-hook 'compilation-filter-hook 'ff/ansi-colorize-buffer)



;; Handle LaTeX compilation errors
(defun compilation-error-latex-file ()
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

;; Add LaTeX errors detection to the list
(add-to-list 'compilation-error-regexp-alist 'latex-error)
(add-to-list 'compilation-error-regexp-alist-alist
             '(latex-error
               "^l\\.\\([[:digit:]]+\\)[[:space:]]" ;; Regular expression
               compilation-error-latex-file         ;; Filename
               1                                    ;; Line number
               nil                                  ;; Column number
               2                                    ;; Type (error)
               1))                                  ;; Highlight



;; Handle LaTeX compilation warnings
(defun ff/compilation-LaTeX-filter ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq buffer-read-only nil)
    (while (re-search-forward "^LaTeX Warning: " nil t)
      (while (not (re-search-forward "on input line [[:digit:]]+\\.$" (line-end-position) t))
        (end-of-line)
        (delete-char 1)
        (beginning-of-line)))
    (setq buffer-read-only t)))
(add-hook 'compilation-filter-hook 'ff/compilation-LaTeX-filter)

(add-to-list 'compilation-error-regexp-alist 'latex-warning)
(add-to-list 'compilation-error-regexp-alist-alist
             '(latex-warning
               "^LaTeX Warning: .* on input line \\([[:digit:]]+\\)\\.$" ;; Regular expression
               compilation-error-latex-file                              ;; Filename
               1                                                         ;; Line number
               nil                                                       ;; Column number
               1))                                                       ;; Type (warning)
