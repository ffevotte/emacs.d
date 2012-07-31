;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local emacs configuration file for clau5abb ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Inconsolata font
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


;; Local installation path
(add-to-list 'load-path "~/.emacs.d/local/cedet-1.1/common")
(add-to-list 'load-path "~/.emacs.d/local/expand-region.el")

;; Local Variables:
;;   mode: emacs-lisp
;; End:
