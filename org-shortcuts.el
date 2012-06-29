;;; org-shortcuts.el --- Clocking shortcuts  

;; Copyright (C) 2011 Benjamin Drieu <bdrieu@april.org>

;; Author: Benjamin Drieu <bdrieu@april.org>
;; Keywords: 
;;
;; This file is NOT part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file contains the code for defining shortcuts to Org-mode
;; clocking.

;; A hook must be added to org-clock-select-task in order to insert
;; shortcuts :

;; (add-hook 'org-clock-select-task-hook 'org-clock-insert-shortcuts)

;; To enter new shortcuts, just add a SHORTCUT property to an entry:
;;
;; :PROPERTIES:
;; :SHORTCUT: x
;; :END:")))
;;
;; After that, when `org-clock-select-task' is called, shortcuts are
;; added so that regularly clocked tasks are shown.  Of course, a
;; shortcut may only be one letter long and "i" and "d" should be
;; avoided or conflicts will happen with default letters.

;;; Code:

(defcustom org-clock-shortcut-property "SHORTCUT"
  "Name of the property that contains shortcuts."
  :type 'string
  :group 'org-clock)

(defun org-clock-insert-shortcuts ()
  "Insert shortcuts to various tasks in current buffer.  Most
probably will this be called from org-clock-select-task via a
hook.

To enter new shortcuts, just add a SHORTCUT property to an entry:

 :PROPERTIES:
 :SHORTCUT: x
 :END:"
  (message "toto")
  (let ((markers nil))
    (save-excursion
      (save-window-excursion
	(mapc
	 (lambda (b)
	   (when (and (with-current-buffer b (org-mode-p))
		      (with-current-buffer b buffer-file-name))
	     (switch-to-buffer b)
	     (org-scan-tags 
	      '(add-to-list 'markers (set-marker (make-marker) (point)))
	      '(org-entry-get nil org-clock-shortcut-property))))
	 (buffer-list))))
    (if markers
	(progn
	  (insert (org-add-props "Shortcuts\n" nil 'face 'bold))
	  (mapc
	   (lambda (m)
	     (when (marker-buffer m)
	       (setq i (1+ i)
		     s (org-clock-insert-selection-line
			(string-to-char (org-entry-get m "SHORTCUT")) m))
	       (if (fboundp 'int-to-char) (setf (car s) (int-to-char (car s))))
	       (push s sel-list)))
	   markers)))))

(provide 'org-shortcuts)
