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
  (save-excursion
    (save-match-data
      (let ((found  nil)
            beg
            filename)
        (while (not found)
          (search-backward "(")
          (condition-case nil
              (scan-sexps (point) 1)
            ((error)
             (setq found t))))
        (setq beg (1+ (point)))
        (re-search-forward "[[:space:]]")
        (setq filename (buffer-substring beg (- (point) 1)))
        (list filename)))))

;; Add LaTeX errors detection to the list
(add-to-list 'compilation-error-regexp-alist 'latex-error)
(add-to-list 'compilation-error-regexp-alist-alist
             '(latex-error
               "^l\\.\\([[:digit:]]+\\)[[:space:]]" ;; Regular expression
               compilation-error-latex-file         ;; Filename
               1                                    ;; Line number
               nil                                  ;; Column number
               2))                                  ;; Type (error)
