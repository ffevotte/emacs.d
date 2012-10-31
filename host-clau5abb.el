;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local emacs configuration file for clau5abb ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bitstream font
(setq default-frame-alist
      '((font-backend . "xft")
        (font . "Bitstream Vera Sans Mono-10")))


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


;; Local installation path
(add-to-list 'load-path "~/.emacs.d/local")
(load-file "/home/H55056/.emacs.d/local/cedet-snapshot-8387/cedet-devel-load.el")


(global-ede-mode 1)
(ede-cpp-root-project "SN2D1D - SVN trunk"
                      :name "SN2D1D - SVN trunk"
                      :file "/home/H55056/local/HPCneutro/sn2d1d.svn/trunk/Makefile.am"
                      :include-path '("./"
                                      "/include/")
                      ;; :system-include-path '("~/exp/include")
                      ;; :spp-table '(("isUnix" . "")
                      ;;              ("BOOST_TEST_DYN_LINK" . ""))
                      )



;; Local Variables:
;;   mode: emacs-lisp
;; End:
