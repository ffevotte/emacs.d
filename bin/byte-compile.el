(defun dest-file-uptodate-p (filename)
  "non-nil if `byte-compile-dest-file' exists and is newer than FILENAME"
  (let ((dest-file (byte-compile-dest-file filename)))
    (when (file-exists-p dest-file)
      (let ((t1 (nth 5 (file-attributes filename)))
            (t2 (nth 5 (file-attributes dest-file))))
        (time-less-p t1 t2)))))

(defun byte-recompile-file (filename)
  "Byte-compile FILENAME only if needed.
Files need recompilation if no `.elc' exist or the `.elc' file is
older than the `.el' file."
  (unless (dest-file-uptodate-p filename)
    (byte-compile-file filename)))

(defun byte-recompile-directory-non-recursive (directory)
  "Byte-compile every `.el' file in DIRECTORY that needs it.
This happens when no `.elc' file exist or the `.elc' file is
older than the `.el' file.

Files in subdirectories of DIRECTORY are not processed."
  (mapc 'byte-recompile-file
        (file-expand-wildcards (concat directory "*.el"))))
