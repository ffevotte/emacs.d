;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local emacs configuration file for clau5abb ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local installation path
(add-to-list 'load-path "~/.emacs.d/local")

(load-file "~/.emacs.d/local/cedet-snapshot-8387/cedet-devel-load.el") ; CEDET

(add-to-list 'load-path "/home/H55056/.emacs.d/predictive")            ; Predictive
(add-to-list 'load-path "/home/H55056/.emacs.d/predictive/texinfo")
(add-to-list 'load-path "/home/H55056/.emacs.d/predictive/latex")
(add-to-list 'load-path "/home/H55056/.emacs.d/predictive/html")
(autoload 'predictive-mode "/home/H55056/.emacs.d/predictive/predictive"
  "Turn on Predictive Completion Mode." t)


;; Bitstream font
(setq default-frame-alist
      '((font-backend . "xft")
        (font . "Inconsolata-11")))


;; custom version of org-clock.el to add org-clock-before-select-task-hook
(eval-after-load "org"
  '(load "org-clock.el"))


;; Org-mode setup
(setq ff/use-org t)
(setq org-agenda-files '("/netdata/H55056/org" "/netdata/H55056/org/notes"))
(setq org-default-notes-file "/netdata/H55056/org/refile.org")
(setq org-default-todo-file "/netdata/H55056/org/todo.org")
(setq diary-file "/netdata/H55056/org/diary")
(setq org-tag-alist
      (quote
       ((:startgroup)("p_sf")("p_sc")("p_hp")("p_non")(:endgroup)
        ("a_ana".?a)("a_maq".?m)("a_dev".?d)("a_etu".?e)("a_red".?r)("a_com".?c)("a_for".?f)("a_adm")
        ("c_cocagne".?C)("c_2d1d".?S)
        ("l_cxx")("l_py")("l_oct")
        ("t_hpc")
        ("short")("export")("noexport")("note"))))
(setq bh/organization-task-id "7da774e7-d69c-463b-88d2-e3ccc03f80fd")


;; Magit
(setq magit-repo-dirs '("~/.emacs.d" "~/.emacs.d/local" "~/.etc" "~/local/perso/projets/git/"))


;; CEDET
(defun ff/generate-ede-projects (command include-path)
  (let ((ede-config-buffer (current-buffer)))
    (with-temp-buffer
      (shell-command command (current-buffer))
      (goto-char 0)
      (let ((again t)
            file)
        (while again
          (setq file (buffer-substring (line-beginning-position) (line-end-position)))
          (if (> (forward-line) 0)
              (setq again nil)
            (print `(ede-cpp-root-project
                     ,file
                     :file ,file
                     :include-path ',include-path)
                   ede-config-buffer)))))))

(defun ff/update-ede-projects ()
  (interactive)
  (with-temp-buffer
    (ff/generate-ede-projects "find ~/local/HPCneutro -name autogen"
                              '("." "/include"))
    (ff/generate-ede-projects "find /scratch/H55056/cocagne -name tests.list"
                              '("." "/../include"))
    (write-file "~/.emacs.d/host-clau5abb-ede.el")))

(global-ede-mode 1)
(unless (load "~/.emacs.d/host-clau5abb-ede.el" 'noerror)
  (ff/update-ede-projects)
  (load "~/.emacs.d/host-clau5abb-ede.el" 'noerror))


;; Local Variables:
;;   mode: emacs-lisp
;; End:
